;; Copyright 2016 Alex Dunn <dunn.alex@gmail.com>

;; This file is part of blockparty.

;; blockparty is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; blockparty is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with blockparty.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:blockparty)

;; Load the OAuth settings from ../oauth.yml
;;
;; yaml:parse returns a hash accessed as
;; (gethash "method" oauth-parameters) => "HMAC-SHA1"
(defvar oauth-parameters
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_pn.htm
  (yaml:parse (make-pathname :directory '(:relative "config")
                             :name "oauth" :type "yml"))
  "OAuth settings for requesting tokens from Twitter.")

(setq chirp-extra:*oauth-api-key* (gethash "client_key" oauth-parameters))
(setq chirp-extra:*oauth-api-secret* (gethash "client_secret" oauth-parameters))

;; (defvar request-alist nil)
;; (defvar mip nil)
;; (handler-case
;;     (setq request-alist (chirp:oauth/request-token (gethash "callback_url" oauth-parameters)))
;;   (chirp:oauth-request-error (err)
;;     (setq mip (chirp:http-body err))))

(defun login ()
  "Get a request token from Twitter, then redirect the user to Twitter
to authorize the application."
  (let (;; `request-alist' remains nil unless we successfully get a request
        ;; token, to which it is assigned
        (request-alist)
        ;; Default to 302 for a redirect to Twitter's auth page in
        ;; the case of a successful call to oauth/request-token
        (response-code 302))
    ;; http://stackoverflow.com/a/13628395
    (handler-case
        (setq request-alist (chirp:oauth/request-token (gethash "callback_url" oauth-parameters)))
      ;; If the request fails, log the error and leave `request-alist'
      ;; unset
      (chirp:oauth-request-error (err)
        ;; Probably 401 in this case
        (setq response-code (chirp:http-status err))
        (hunchentoot:acceptor-log-message
         ;; *acceptor* is a magic variable assigned to the acceptor
         ;; calling this handler:
         ;; http://weitz.de/hunchentoot/#*acceptor*
         hunchentoot:*acceptor*
         :error
         (format nil "~d: ~a"
                 (chirp:http-status err)
                 (cdr (assoc :message (car (cdr (assoc :errors (chirp:http-body err))))))))))
    ;; *reply* is another magic variable assigned to the object
    ;; Hunchentoot uses to build its response to requests:
    ;; http://weitz.de/hunchentoot/#*reply*
    (setf (hunchentoot:return-code hunchentoot:*reply*) response-code)
    (if request-alist
        (let* ((uuid (write-to-string (unicly:make-v4-uuid)))
               (request-token (cdr (assoc :oauth-token request-alist)))
               (request-secret (cdr (assoc :oauth-token-secret request-alist)))
               ;; These are stored in Redis
               (token-cookie-key (concatenate 'string uuid ":token"))
               (secret-cookie-key (concatenate 'string uuid ":secret"))
               (cookie (hunchentoot:set-cookie
                        "request-id"
                        :value uuid
                        :max-age 60
                        :path "/"
                        :secure (equal "production" (getenv "LISP_ENV"))
                        :http-only t)))
          ;; Hunchentoot has it's own full-fledged session handling
          ;; but I'm not using it since I don't really understand how
          ;; it works: http://weitz.de/hunchentoot/#sessions
          ;;
          ;; Instead we'll just use the cookie functions and manage
          ;; sessions ourselves in Redis.
          (hunchentoot:set-cookie* cookie)
          (redis:with-connection ()
            (redis:with-pipelining
              (red:set token-cookie-key request-token)
              (red:expire token-cookie-key 60)
              (red:set secret-cookie-key request-secret)
              (red:expire secret-cookie-key 60)))

          (setf (hunchentoot:return-code hunchentoot:*reply*) 302)
          (setf (hunchentoot:header-out "Location" hunchentoot:*reply*)
                (concatenate 'string "https://api.twitter.com/oauth/authenticate?oauth_token=" request-token))
          "Redirecting to Twitter dot com...")
      ;; TODO: Make a real error page + message
      "An error occured.")))

(defun auth ()
  "When arriving from Twitter with a valid oauth_verifier, get an
access token from Twitter."
  (let* (;; `access-alist' remains nil unless we successfully get an access
         ;; token, to which it is assigned
         (access-alist)
         ;; *response* is a magic variable assigned to the response
         ;; *received by the handler
         (params (hunchentoot:get-parameters* hunchentoot:*request*))
         (session-id (hunchentoot:cookie-in "session-id" hunchentoot:*request*))
         (oauth-verifier (cdr (assoc "oauth_verifier" params :test #'string=))))
    (if oauth-verifier
        (let* ((request-id (hunchentoot:cookie-in "request-id" hunchentoot:*request*))
               ;; Why are we setting the /access/ variables to the
               ;; temporary /request/ tokens?  No idea, but apparently
               ;; that's how Chirp does it.
               (chirp-extra:*oauth-access-token*
                (red:get (concatenate 'string request-id ":token")))
               (chirp-extra:*oauth-access-secret*
                (red:get (concatenate 'string request-id ":secret"))))
          (handler-case
              (setq access-alist (chirp:oauth/access-token oauth-verifier))
            (chirp:oauth-request-error (err)
              (setq response-code (chirp:http-status err))
              (hunchentoot:acceptor-log-message
               ;; *acceptor* is a magic variable assigned to the
               ;; acceptor calling this handler:
               ;; http://weitz.de/hunchentoot/#*acceptor*
               hunchentoot:*acceptor* :error (format nil "~a" err))))
          (if access-alist
              (let* ((access-token (cdr (assoc "OAUTH-TOKEN" access-alist :test #'string=)))
                     (access-secret (cdr (assoc "OAUTH-TOKEN-SECRET" access-alist :test #'string=)))
                     (session-id (write-to-string (unicly:make-v4-uuid)))
                     (salt (ironclad:hex-string-to-byte-array (red:get "salt")))
                     ;; Key base for Redis
                     (passwd (ironclad:pbkdf2-hash-password-to-combined-string
                              (ironclad:ascii-string-to-byte-array session-id)
                              :salt salt))
                     ;; Keys for Redis
                     (cookie-passwd (concatenate 'string session-id ":passwd"))
                     (cookie-token (concatenate 'string session-id ":token"))
                     (cookie-secret (concatenate 'string session-id ":secret"))
                     (cookie (hunchentoot:set-cookie
                              "session-id"
                              :value session-id
                              :max-age 86400
                              :path "/"
                              :domain (or (getenv "BP_DOMAIN") "localhost")
                              :secure (equal "production" (getenv "BP_ENV"))
                              :http-only t)))

                ;; (hunchentoot:acceptor-log-message
                ;;  hunchentoot:*acceptor* :debug (format nil "~a" (hunchentoot:headers-in*)))
                ;; (hunchentoot:acceptor-log-message
                ;;  hunchentoot:*acceptor* :debug (format nil "session: ~a" session-id))
                ;; (hunchentoot:acceptor-log-message
                ;;  hunchentoot:*acceptor* :debug (format nil "passwd: ~a" passwd))

                (hunchentoot:set-cookie* cookie)
                (redis:with-connection ()
                  (redis:with-pipelining
                    (red:set cookie-passwd passwd)
                    (red:expire cookie-passwd 86400)
                    (red:set cookie-token access-token)
                    (red:expire cookie-token 86400)
                    (red:set cookie-secret access-secret)
                    (red:expire cookie-secret 86400)))

                (hunchentoot:acceptor-log-message
                 hunchentoot:*acceptor* :info
                 (format nil "Authenticated with Twitter, starting session ~a" session-id))

                (setf (hunchentoot:return-code hunchentoot:*reply*) 302)
                (setf (hunchentoot:header-out "Location" hunchentoot:*reply*) "/")
                "You're logged in!")

            ;; If authentication fails, clear the current session
            (progn
              (delete-session session-id)
              (view/index
               nil
               '("error" . "Failed to get an access token. Please try again or open an issue.")))))
      ;; If authentication fails, clear the current session
      (progn
        (delete-session session-id)
        (view/index
         nil
         '("error" . "Failed to get a verifier token. Please try again or open an issue."))))))

(defun entry ()
  "The view rendered at /."
  (setf (hunchentoot:content-type*) "text/html")
  (view/index))
