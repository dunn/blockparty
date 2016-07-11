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
  (let* ((user-id (validate-session (tbnl:cookie-in "session-id" tbnl:*request*))))
    ;; (tbnl:acceptor-log-message
    ;;  tbnl:*acceptor* :debug (format nil "headers: ~a" (tbnl:headers-in*)))
    (if user-id
        (html/page "Block Party"
                   (concatenate 'string "<p>Welcome number " user-id "</p>")
                   class flash)
      (html/page
       "Blick Party"
       "<p><a href='/login' title='Log in please'>Log in now</a></p>" class flash))))
