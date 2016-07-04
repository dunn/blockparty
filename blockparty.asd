;;;; blockparty.asd

(asdf:defsystem #:blockparty
  :description "Twitter block manager"
  :author "Alex Dunn <dunn.alex@gmail.com>"
  :license "GPLv3"
  :depends-on (#:cl-twitter
               #:hunchentoot
               #:north
               #:ironclad)
  :serial t
  :components ((:file "package")
               (:module "lib"
                        :serial t
                        :components ((:file "html")
                                     (:file "handlers")))
               (:module "views"
                        :components ((:file "index")))
               (:file "blockparty")))