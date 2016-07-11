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
         ;; *request* is a magic variable assigned to the request
         ;; *received by the handler
         (params (tbnl:get-parameters* tbnl:*request*))
         (session-id (tbnl:cookie-in "session-id" tbnl:*request*))
         (oauth-verifier (cdr (assoc "oauth_verifier" params :test #'string=))))
    (if oauth-verifier
        (let* ((request-id (tbnl:cookie-in "request-id" tbnl:*request*))
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
              (tbnl:acceptor-log-message
               ;; *acceptor* is a magic variable assigned to the
               ;; acceptor calling this handler:
               ;; http://weitz.de/hunchentoot/#*acceptor*
               tbnl:*acceptor* :error (format nil "~a" err))))
          (if access-alist
              (let* ((access-token (cdr (assoc "OAUTH-TOKEN" access-alist :test #'string=)))
                     (access-secret (cdr (assoc "OAUTH-TOKEN-SECRET" access-alist :test #'string=)))
                     (user-id (cdr (assoc "USER-ID" access-alist :test #'string=)))
                     (session-id (write-to-string (uuid:make-v4-uuid)))
                     (salt (ironclad:hex-string-to-byte-array (red:get "salt")))
                     (passwd (ironclad:pbkdf2-hash-password-to-combined-string
                              (ironclad:ascii-string-to-byte-array session-id)
                              :salt salt))
                     ;; Keys for Redis
                     (cookie-id-key (concatenate 'string session-id ":id"))
                     (cookie-passwd-key (concatenate 'string session-id ":passwd"))
                     (cookie-token-key (concatenate 'string session-id ":token"))
                     (cookie-secret-key (concatenate 'string session-id ":secret"))
                     (cookie (tbnl:set-cookie
                              "session-id"
                              :value session-id
                              :max-age 86400
                              :path "/"
                              :domain (or (getenv "BP_DOMAIN") "localhost")
                              :secure (equal "production" (getenv "BP_ENV"))
                              :http-only t)))

                ;; (tbnl:acceptor-log-message
                ;;  tbnl:*acceptor* :debug (format nil "~a" (tbnl:headers-in*)))
                ;; (tbnl:acceptor-log-message
                ;;  tbnl:*acceptor* :debug (format nil "session: ~a" session-id))
                ;; (tbnl:acceptor-log-message
                ;;  tbnl:*acceptor* :debug (format nil "passwd: ~a" passwd))

                (tbnl:set-cookie* cookie)
                (redis:with-connection ()
                  (redis:with-pipelining
                    (red:set cookie-id-key user-id)
                    (red:expire cookie-id-key 86400)
                    (red:set cookie-passwd-key passwd)
                    (red:expire cookie-passwd-key 86400)
                    (red:set cookie-token-key access-token)
                    (red:expire cookie-token-key 86400)
                    (red:set cookie-secret-key access-secret)
                    (red:expire cookie-secret-key 86400)))

                (tbnl:acceptor-log-message
                 tbnl:*acceptor* :info
                 (format nil "Authenticated with Twitter, starting session ~a" session-id))

                (setf (tbnl:return-code tbnl:*reply*) 302)
                (setf (tbnl:header-out "Location" tbnl:*reply*) "/")
                "You're logged in!")

            ;; If authentication fails, clear the current session
            (progn
              (delete-session session-id)
              (setf (tbnl:return-code tbnl:*reply*) 401)
              (view/index
               nil
               '((:mode . "error")
                 (:message . "Failed to get an access token. Please try again or open an issue."))))))
      ;; If authentication fails, clear the current session
      (progn
        (delete-session session-id)
        (setf (tbnl:return-code tbnl:*reply*) 401)
        (view/index
         nil
         '((:mode . "error")
           (:message . "Failed to get a verifier token. Please try again or open an issue.")))))))
