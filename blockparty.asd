;;;; blockparty.asd

(asdf:defsystem #:blockparty
  :description "Twitter block manager"
  :author "Alex Dunn <dunn.alex@gmail.com>"
  :license "GPLv3"
  :depends-on (#:chirp
               #:cl-redis
               #:cl-yaml
               #:hunchentoot
               #:ironclad
               #:lisp-unit
               #:postmodern
               #:uuid)
  :serial t
  :components ((:file "package")
               (:module "lib"
                        :components ((:file "html")
                                     (:file "util")))
               (:module "lib/handlers"
                        :components ((:file "callback")
                                     (:file "login")
                                     (:file "root")))
               (:module "views"
                        :components ((:file "index")))
               (:module "tests"
                        :components ((:file "util-test")))
               (:file "blockparty")))
