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
               #:postmodern
               #:unicly)
  :serial t
  :components ((:file "package")
               (:module "lib"
                        :components ((:file "handlers")
                                     (:file "html")
                                     (:file "util")))
               (:module "views"
                        :components ((:file "index")))
               (:file "blockparty")))
