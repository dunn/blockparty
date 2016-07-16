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

(defun get-app-var (variable)
  "Return the value of VARIABLE, first checking the environment then
  checking `*oauth-config*'.  Return nil if unset."
  (or (getenv variable)
      ;; *oauth-config* is a magic variable set by `main'
      (gethash variable *oauth-config*)))

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
        (red:del (concatenate 'string session-id ":screen-name"))
        (red:del (concatenate 'string session-id ":secret"))
        (red:del (concatenate 'string session-id ":token"))
        (red:del (concatenate 'string session-id ":user-id"))))))

(defun validate-session (session-id passwd salt)
  "Validate the SESSION-ID against the provided SALT.  Return t if so,
otherwise nil."
  ;; We're guarding here because usually what's passed in is the
  ;; output of `red:get' which might well be nil.
  (when session-id
    (when (stringp salt)
      ;; NB: hex-string, /not/ ascii-string!
      (setq salt (ironclad:hex-string-to-byte-array salt)))

    (let ((session-vector (ironclad:ascii-string-to-byte-array session-id)))
      ;; (tbnl:acceptor-log-message
      ;;  tbnl:*acceptor* :debug (format nil "old passwd: ~a" passwd))
      ;; (tbnl:acceptor-log-message
      ;;  tbnl:*acceptor* :debug (format nil "new passwd: ~a" (ironclad:pbkdf2-hash-password-to-combined-string
      ;;                                                       session-vector :salt salt)))
      (when (equal passwd
                   (ironclad:pbkdf2-hash-password-to-combined-string
                    session-vector :salt salt))
        t))))

(defun make-session (salt)
  "Using the provided SALT, return a hash table containing a unique
  session ID and a salted password."
  ;; (tbnl:acceptor-log-message
  ;;  tbnl:*acceptor* :debug (format nil "salt in: ~a" salt))
  (when (stringp salt)
    (setq salt (ironclad:hex-string-to-byte-array salt)))
  ;; (tbnl:acceptor-log-message
  ;;  tbnl:*acceptor* :debug (format nil "salt out: ~a" salt))
  (let* ((session-id (write-to-string (uuid:make-v4-uuid)))
         (passwd (ironclad:pbkdf2-hash-password-to-combined-string
                  (ironclad:ascii-string-to-byte-array session-id)
                  :salt salt))
         (session-table (make-hash-table)))
    (setf (gethash :id session-table) session-id)
    (setf (gethash :passwd session-table) passwd)
    session-table))
