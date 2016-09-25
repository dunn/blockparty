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
;;     (setq request-alist (chirp:oauth/request-token (get-app-var "callback_url")))
;;   (chirp:oauth-request-error (err)
;;     (setq mip (chirp:http-body err))))

(defun handle/login ()
  "Get a request token from Twitter, then redirect the user to Twitter
to authorize the application."
  (let ((chirp-extra:*oauth-api-key* (get-app-var "client_key"))
        (chirp-extra:*oauth-api-secret* (get-app-var "client_secret"))
        ;; `request-alist' remains nil unless we successfully get a request
        ;; token, to which it is assigned
        (request-alist)
        ;; Default to 302 for a redirect to Twitter's auth page in
        ;; the case of a successful call to oauth/request-token
        (response-code 302))

    ;; http://stackoverflow.com/a/13628395
    (handler-case
        (setq request-alist (chirp:oauth/request-token (get-app-var "callback_url")))
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
        (let* ((uuid (write-to-string (uuid:make-v4-uuid)))
               ;; This is the unauthorized request token that Twitter
               ;; provides in response to `chirp:oauth/request-token';
               ;; see https://oauth.net/core/1.0a/#auth_step1
               (request-token (cdr (assoc :oauth-token request-alist)))
               ;;
               ;; The OAuth 1.0 spec requires that the request token
               ;; have a corresponding secret value, so that a
               ;; third-party can't intercept the oauth_verifier (see
               ;; https://oauth.net/core/1.0a/#auth_step3) and get an
               ;; access token; however, Twitter does not make use of
               ;; the oauth_token_secret: http://stackoverflow.com/a/9857158
               ;;
               ;; (request-secret (cdr (assoc :oauth-token-secret request-alist)))

               ;; Keys for storing tokens in in Redis
               (token-cookie-key (concatenate 'string uuid ":token"))
               ;; (secret-cookie-key (concatenate 'string uuid ":secret"))
               (cookie (tbnl:set-cookie
                        "request-id"
                        :value uuid
                        ;; Give them a minute to log in to Twitter
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

          ;; When the Request Token is authorized, the response from
          ;; Twitter includes the token, so this isn't strictly
          ;; necessary; but pinning it to a cookie makes it a little
          ;; harder for a third party to swipe the token and verifier
          ;; and get an Access Token.
          (redis:with-connection ()
            (redis:with-pipelining
              (red:set token-cookie-key request-token)
              (red:expire token-cookie-key 60)
              ;; (red:set secret-cookie-key request-secret)
              ;; (red:expire secret-cookie-key 60)
              ))

          (setf (tbnl:return-code tbnl:*reply*) 302)
          (setf (tbnl:header-out "Location" tbnl:*reply*)
                (concatenate 'string "https://api.twitter.com/oauth/authenticate?oauth_token=" request-token))
          "Redirecting to Twitter dot com...")
      (view/index
       nil
       '((:mode . "error")
         (:message . "Failed to get a request token. Please try again or open an issue."))))))
