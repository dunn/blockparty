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
  (let* ((chirp-extra:*oauth-api-key* (oauth-var "client_key"))
         (chirp-extra:*oauth-api-secret* (oauth-var "client_secret"))
         ;; `access-alist' remains nil unless we successfully get an access
         ;; token, to which it is assigned
         (access-alist)
         ;; *request* is a magic variable assigned to the request
         ;; *received by the handler
         (params (tbnl:get-parameters* tbnl:*request*))
         (oauth-verifier (cdr (assoc "oauth_verifier" params :test #'string=))))

    (if (not oauth-verifier)
        (progn
          (setf (tbnl:return-code tbnl:*reply*) 401)
          (view/login
           nil
           '((:mode . "error")
             (:message . "Failed to get a verifier token. Please try again or open an issue."))))
      (let* (
             ;; The cookie corresponding to the Request Token.  When the
             ;; Request Token is authorized, the response from Twitter
             ;; includes the token, so this isn't strictly necessary; but
             ;; pinning it to a cookie makes it a little harder for a
             ;; third party to swipe the token and verifier and get an
             ;; Access Token.
             (request-id (tbnl:cookie-in "request-id" tbnl:*request*))
             ;; Why are we setting an /access/ token variable to the
             ;; temporary /request/ token?  No idea, but apparently that's
             ;; how Chirp does it.
             (chirp-extra:*oauth-access-token*
              (red:get (concatenate 'string request-id ":token"))))
        (if (not chirp-extra:*oauth-access-token*)
            (progn
              (setf (tbnl:return-code tbnl:*reply*) 401)
              (view/login
               nil
               '((:mode . "error")
                 (:message . "Took too long to authenticate with Twitter. Please try again or open an issue."))))
          (progn
            ;; Request an Access Token from Twitter, handling any error
            ;; responses.  See https://oauth.net/core/1.0a/#auth_step3
            (handler-case
                (setq access-alist (chirp:oauth/access-token oauth-verifier))
              (chirp:oauth-request-error (err)
                (setq response-code (chirp:http-status err))
                (tbnl:acceptor-log-message
                 ;; *acceptor* is a magic variable assigned to the
                 ;; acceptor calling this handler:
                 ;; http://weitz.de/hunchentoot/#*acceptor*
                 tbnl:*acceptor* :error (format nil "~a" err))))

            (if (not access-alist)
                (progn
                  (setf (tbnl:return-code tbnl:*reply*) 401)
                  (view/login
                   nil
                   '((:mode . "error")
                     (:message . "Failed to get an access token. Please try again or open an issue."))))

              (let* (;; The alist returned by oauth/access-token has
                     ;; uppercased keys
                     (access-token (cdr (assoc "OAUTH-TOKEN" access-alist :test #'string=)))
                     (access-secret (cdr (assoc "OAUTH-TOKEN-SECRET" access-alist :test #'string=)))
                     (user-id (cdr (assoc "USER-ID" access-alist :test #'string=)))
                     (screen-name (cdr (assoc "SCREEN-NAME" access-alist :test #'string=)))
                     ;; NB: hex-string, /not/ ascii-string!
                     (salt (ironclad:hex-string-to-byte-array (red:get "salt")))
                     (session (make-session salt))
                     (session-id (gethash :id session))
                     ;; Keys for Redis
                     (cookie (tbnl:set-cookie
                              "session-id"
                              :value session-id
                              :max-age 86400
                              :path "/"
                              :domain (or (uiop:getenv "BP_DOMAIN") "localhost")
                              :secure (equal "production" (uiop:getenv "BP_ENV"))
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
                      (red:setex (concatenate 'string session-id ":passwd") 86400 (gethash :passwd session))
                    (red:setex (concatenate 'string session-id ":screen-name") 86400 screen-name)
                    (red:setex (concatenate 'string session-id ":secret") 86400 access-secret)
                    (red:setex (concatenate 'string session-id ":token") 86400 access-token)
                    (red:setex (concatenate 'string session-id ":user-id") 86400 user-id)))

                (tbnl:acceptor-log-message
                 tbnl:*acceptor* :info
                 (format nil "Authenticated with Twitter, starting session ~a" session-id))

                (setf (tbnl:return-code tbnl:*reply*) 302)
                (setf (tbnl:header-out "Location" tbnl:*reply*) "/")
                "You're logged in!"))))))))
