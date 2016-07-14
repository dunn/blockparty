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

;; Some implementation details from
;; http://cl-cookbook.sourceforge.net/os.html
(defun getenv (variable)
  "Get an environment VARIABLE.
If the variable is unset return nil."
  #+ABCL (ext:getenv variable)
  #+Allegro (sys:getenv variable)
  #+CCL (ccl:getenv variable)
  #+CMU (cdr (assoc variable ext:*environment-list* :test #'string=))
  #+ECL (si:getenv variable)
  #+LISPWORKS (lispworks:environment-variable variable)
  #+SBCL (sb-unix::posix-getenv variable)
  )

(defun delete-session (session-id)
  "Erase the SESSION-ID from Redis. Assumes an open Redis connection."
  (when session-id
    (when (boundp 'tbnl:*acceptor*)
      (tbnl:acceptor-log-message
       tbnl:*acceptor* :info
       (format nil "Deleting session ~a" session-id)))
    (redis:with-connection ()
      (redis:with-pipelining
        (red:del (concatenate 'string session-id ":passwd"))
        (red:del (concatenate 'string session-id ":token"))
        (red:del (concatenate 'string session-id ":secret"))))))

(defun validate-session (session-id)
  "Extract the session-id from the HEADERS and check if it's still
valid.  Returns the user ID if so, otherwise nil.  Assumes an open
Redis connection."
  (when session-id
    (let ((session-vector (ironclad:ascii-string-to-byte-array session-id))
          (session-passwd (red:get (concatenate 'string session-id ":passwd")))
          (salt (ironclad:hex-string-to-byte-array (red:get "salt"))))
      ;; (tbnl:acceptor-log-message
      ;;  tbnl:*acceptor* :debug (format nil "old passwd: ~a" session-passwd))
      ;; (tbnl:acceptor-log-message
      ;;  tbnl:*acceptor* :debug (format nil "new passwd: ~a" (ironclad:pbkdf2-hash-password-to-combined-string
      ;;                                                       session-vector :salt salt)))
      (when (equal session-passwd
                   (ironclad:pbkdf2-hash-password-to-combined-string
                    session-vector :salt salt))
        ;; (tbnl:acceptor-log-message
        ;;  tbnl:*acceptor* :debug
        ;;  (format nil "id: ~a" (red:get (concatenate 'string session-id ":id"))))
        (red:get (concatenate 'string session-id ":id"))))))
