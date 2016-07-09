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

;; (gethash '"client_secret" oauth-parameters)

(defvar request-secret nil
  "Temporary secret credential for identifying the access request to Twitter.
Declared outside of the handler functions since it needs to be
available to both `login' and `auth'.")

(defun login ()
  "Get a request token from Twitter, then redirect the user to Twitter
to authorize the application."
  (let ((chirp-extra:*oauth-api-key* (gethash "client_key" oauth-parameters))
        (chirp-extra:*oauth-api-secret* (gethash "client_secret" oauth-parameters))
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
         ;; Magic variable assigned to the acceptor calling this
         ;; handler: http://weitz.de/hunchentoot/#*acceptor*
         hunchentoot:*acceptor*
         :error
         (format nil "~d: ~a"
                 (chirp:http-status err)
                 (cdr (caadar (chirp:http-body err)))))))
    ;; *reply* is another magic variable assigned to the object
    ;; Hunchentoot uses to build its response to requests:
    ;; http://weitz.de/hunchentoot/#*reply*
    (setf (hunchentoot:return-code hunchentoot:*reply*) response-code)
    (if request-alist
        (progn
          (setq request-secret (assoc :oauth-token-secret request-alist))
          (setf (hunchentoot:return-code hunchentoot:*reply*) 302)
          (setf (hunchentoot:header-out "Location" hunchentoot:*reply*)
                (concatenate 'string
                             "https://api.twitter.com/oauth/authenticate?oauth_token="
                             (cdr (assoc :oauth-token request-alist))))
          "Redirecting to Twitter dot com...")
      ;; TODO: Make a real error page + message
      "An error occured.")))

(defun auth ()
  "Not implemented."
  )

(defun index ()
  "The view rendered at /."
  (setf (hunchentoot:content-type*) "text/html")
  (view/index))
