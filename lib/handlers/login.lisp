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

;; (defvar request-alist nil)
;; (defvar mip nil)
;; (handler-case
;;     (setq request-alist (chirp:oauth/request-token (gethash "callback_url" *oauth-config*)))
;;   (chirp:oauth-request-error (err)
;;     (setq mip (chirp:http-body err))))

(defun handle/login ()
  "Get a request token from Twitter, then redirect the user to Twitter
to authorize the application."
  (let (;; *oauth-config* is a magic variable set by `main'
        (chirp-extra:*oauth-api-key* (gethash "client_key" *oauth-config*))
        (chirp-extra:*oauth-api-secret* (gethash "client_secret" *oauth-config*))
        ;; `request-alist' remains nil unless we successfully get a request
        ;; token, to which it is assigned
        (request-alist)
        ;; Default to 302 for a redirect to Twitter's auth page in
        ;; the case of a successful call to oauth/request-token
        (response-code 302))
    ;; http://stackoverflow.com/a/13628395
    (handler-case
        (setq request-alist (chirp:oauth/request-token (gethash "callback_url" *oauth-config*)))
      ;; If the request fails, log the error and leave `request-alist'
      ;; unset
      (chirp:oauth-request-error (err)
        ;; Probably 401 in this case
        (setq response-code (chirp:http-status err))
        (tbnl:acceptor-log-message
         ;; *acceptor* is a magic variable assigned to the acceptor
         ;; calling this handler:
         ;; http://weitz.de/hunchentoot/#*acceptor*
         tbnl:*acceptor*
         :error
         (format nil "~d: ~a"
                 (chirp:http-status err)
                 (cdr (assoc :message (car (cdr (assoc :errors (chirp:http-body err))))))))))
    ;; *reply* is another magic variable assigned to the object
    ;; Hunchentoot uses to build its response to requests:
    ;; http://weitz.de/hunchentoot/#*reply*
    (setf (tbnl:return-code tbnl:*reply*) response-code)
    (if request-alist
        (let* ((uuid (write-to-string (unicly:make-v4-uuid)))
               (request-token (cdr (assoc :oauth-token request-alist)))
               (request-secret (cdr (assoc :oauth-token-secret request-alist)))
               ;; These are stored in Redis
               (token-cookie-key (concatenate 'string uuid ":token"))
               (secret-cookie-key (concatenate 'string uuid ":secret"))
               (cookie (tbnl:set-cookie
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
          (tbnl:set-cookie* cookie)
          (redis:with-connection ()
            (redis:with-pipelining
              (red:set token-cookie-key request-token)
              (red:expire token-cookie-key 60)
              (red:set secret-cookie-key request-secret)
              (red:expire secret-cookie-key 60)))

          (setf (tbnl:return-code tbnl:*reply*) 302)
          (setf (tbnl:header-out "Location" tbnl:*reply*)
                (concatenate 'string "https://api.twitter.com/oauth/authenticate?oauth_token=" request-token))
          "Redirecting to Twitter dot com...")
      (view/html
       nil
       '((:mode . "error")
         (:message . "Failed to get a request token. Please try again or open an issue."))))))
