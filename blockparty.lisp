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

(defun main ()
  "Start the server."
  (defvar blockparty (make-instance
                       'hunchentoot:easy-acceptor
                       :port 3000
                       :document-root "www/"
                       :access-log-destination "log/access.log"
                       :message-log-destination "log/message.log"))

  (setq hunchentoot:*dispatch-table*
        (list
         (hunchentoot:create-regex-dispatcher "^/login/?$" 'login)
         (hunchentoot:create-regex-dispatcher "^/auth/?$" 'auth)
         (hunchentoot:create-regex-dispatcher "^/$" 'entry)))

  (redis:connect)
  (red:set "salt"
           (ironclad:byte-array-to-hex-string
            (ironclad:make-random-salt)))

  (ironclad:byte-array-to-hex-string
   (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array "honk")
                                  :salt (ironclad:ascii-string-to-byte-array "shit")))

  (ironclad:pbkdf2-hash-password-to-combined-string
   (ironclad:ascii-string-to-byte-array "honk")
   :salt (ironclad:ascii-string-to-byte-array "shit"))

  (hunchentoot:start blockparty))
