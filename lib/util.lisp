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

;; Implementation details from
;; http://cl-cookbook.sourceforge.net/os.html
(defun getenv (variable)
  "Get an environment VARIABLE.
If the variable is unset return nil."
  #+Allegro (sys:getenv variable)
  #+CMU (cdr (assoc variable ext:*environment-list* :test #'string=))
  #+ECL (si:getenv variable)
  #+LISPWORKS (lispworks:environment-variable variable)
  #+SBCL (sb-unix::posix-getenv variable)
  )

(defun delete-session (session-id)
  "Erase the SESSION-ID from Redis."
  (when session-id
    (hunchentoot:acceptor-log-message
     hunchentoot:*acceptor* :info
     (format nil "Deleting session ~a" session-id))
    (redis:with-connection ()
      (redis:with-pipelining
        (red:del (concatenate 'string session-id ":passwd"))
        (red:del (concatenate 'string session-id ":token"))
        (red:del (concatenate 'string session-id ":secret"))))))
