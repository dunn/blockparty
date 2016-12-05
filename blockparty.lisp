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

  ;; Load settings from config/*.yml
  (setq *oauth-config* (get-config "oauth"))
  (setq *db-config* (get-config "database"))

  (setq tbnl:*dispatch-table*
        (list
         (tbnl:create-regex-dispatcher "^/$" 'handle/root)
         (tbnl:create-regex-dispatcher "^/callback/?$" 'handle/callback)
         (tbnl:create-regex-dispatcher "^/login/?$" 'handle/login)
         (tbnl:create-regex-dispatcher "^/logout/?$" 'handle/logout)
         ))

  (pomo:clear-connection-pool)
  (setf pomo:*database*
        (pomo:connect (gethash "database" *db-config*)
                      (gethash "user" *db-config*)
                      (gethash "password" *db-config*)
                      "localhost"
                      :pooled-p t))

  (redis:connect)
  (setf *random-state* (make-random-state t))
  (red:set "salt" (ironclad:byte-array-to-hex-string
                   (ironclad:make-random-salt)))
  (tbnl:start blockparty)
  (unless (equal (uiop:getenv "LISP_ENV") "development")
    (loop (sleep 60))))
