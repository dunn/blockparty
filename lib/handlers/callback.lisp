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

(defun handle/callback ()
  "When arriving from Twitter with a valid oauth_verifier, get an
access token from Twitter."
  (let* ((chirp-extra:*oauth-api-key* (gethash "client_key" *oauth-config*))
         (chirp-extra:*oauth-api-secret* (gethash "client_secret" *oauth-config*))
         ;; `access-alist' remains nil unless we successfully get an access
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
              (setf (hunchentoot:return-code hunchentoot:*reply*) 401)
              (view/index
               nil
               '((:mode . "error")
                 (:message . "Failed to get an access token. Please try again or open an issue."))))))
      ;; If authentication fails, clear the current session
      (progn
        (delete-session session-id)
        (setf (hunchentoot:return-code hunchentoot:*reply*) 401)
        (view/index
         nil
         '((:mode . "error")
           (:message . "Failed to get a verifier token. Please try again or open an issue.")))))))
