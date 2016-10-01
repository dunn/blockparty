;;;; blockparty.lisp

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

(defun main (argv)
  "Start the server, ignoring ARGV"
  (declare (ignore argv))
  (defvar blockparty (make-instance
                       'tbnl:easy-acceptor
                       :port 3000
                       :document-root "www/"
                       :access-log-destination "log/access.log"
                       :message-log-destination "log/message.log"))

  ;; Load the OAuth settings from config/oauth.yml
  ;;
  ;; yaml:parse returns a hash accessed as
  ;; (gethash "method" oauth-parameters) => "HMAC-SHA1"
  (setq *oauth-config*
        ;; http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_pn.htm
        (yaml:parse (make-pathname :directory '(:relative "config")
                                     :name "oauth" :type "yml")))

  (setq tbnl:*dispatch-table*
        (list
         (tbnl:create-regex-dispatcher "^/$" 'handle/root)
         (tbnl:create-regex-dispatcher "^/auth/?$" 'handle/callback)
         (tbnl:create-regex-dispatcher "^/login/?$" 'handle/login)
         (tbnl:create-regex-dispatcher "^/logout/?$" 'handle/logout)
         ))

  (redis:connect)
  (setf *random-state* (make-random-state t))
  (red:set "salt" (ironclad:byte-array-to-hex-string
                   (ironclad:make-random-salt)))
  (tbnl:start blockparty)
  (unless (equal (getenv "LISP_ENV") "development")
    (loop (sleep 60))))
