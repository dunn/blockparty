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

(load "quicklisp/setup.lisp")
(ql:quickload 'hunchentoot)

(defvar party (make-instance
               'hunchentoot:easy-acceptor
                :port 3000
                :document-root "www/"
                :access-log-destination "log/access.log"
                :message-log-destination "log/message.log"))

(setq hunchentoot:*dispatch-table*
      (list
       (hunchentoot:create-regex-dispatcher "^/toot" 'toot)))

(defun toot ()
  "Toot"
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "fart"))

(hunchentoot:start party)
