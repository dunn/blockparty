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

(defun handle/root ()
  "The handler for /."
  (let* ((session-id (tbnl:cookie-in "session-id" tbnl:*request*))
         (salt (red:get "salt"))
         (passwd (red:get (concatenate 'string session-id ":passwd")))
         (screen-name (red:get (concatenate 'string session-id ":screen-name")))
         (valid (validate-session session-id passwd salt)))
;;     (tbnl:acceptor-log-message
;;      tbnl:*acceptor* :debug (format nil "view/index session: ~a
;; view/index salt: ~a
;; " session-id salt))
    (if valid
        (view/main screen-name)
      (view/login))))
