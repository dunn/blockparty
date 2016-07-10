;; Copyright 2016 Alex Dunn <dunn.alex@gmail.com>

;; This file is part of block-party.

;; block-party is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; block-party is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with read-thing.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:blockparty)

(defun view/index (&optional class flash)
  "The view rendered at the root."
  (let* ((session-id (hunchentoot:cookie-in "session-id" hunchentoot:*request*))
         (session-vector (ironclad:ascii-string-to-byte-array session-id))
         (session-passwd (red:get (concatenate 'string session-id ":passwd"))))

    ;; (hunchentoot:acceptor-log-message
    ;;  hunchentoot:*acceptor* :debug (format nil "headers: ~a" (hunchentoot:headers-in*)))
    ;; (hunchentoot:acceptor-log-message
    ;;  hunchentoot:*acceptor* :debug (format nil "session-id ~a, passwd ~a" session-id session-passwd))

    (if (and session-passwd
             (ironclad:pbkdf2-check-password session-vector session-passwd))
        (html/page "Block Party" "<p>Logged in my dude</p>" class flash)
      (html/page
       "Blick Party"
       "<p><a href='/login' title='Log in please'>Log in now</a></p>" class flash))))
