(load "quicklisp/setup.lisp")
(ql:quickload 'hunchentoot)

(defvar party (make-instance
               'hunchentoot:easy-acceptor
                :port 3000
                :document-root "www/"
                :access-log-destination "log/access.log"
                :message-log-destination "log/error.log"))

(setq hunchentoot:*dispatch-table*
      (list
       (hunchentoot:create-regex-dispatcher "^/toot" 'toot)))

(defun toot ()
  "Toot"
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "fart"))

(hunchentoot:start party)
