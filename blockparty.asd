;;;; blockparty.asd

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

(require 'asdf)

(asdf:initialize-source-registry
 `(:source-registry
   :ignore-inherited-configuration
   (:tree (:here ".dependencies/packages/"))))

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
               #:uiop
               #:uuid)
  :serial t
  :components ((:file "package")
               (:module "lib"
                        :components ((:file "util")
                                     (:file "handlers/callback")
                                     (:file "handlers/login")
                                     (:file "handlers/logout")
                                     (:file "handlers/root")))
               (:module "views"
                        :components ((:file "login")
                                     (:file "main")
                                     (:file "partials/forms")
                                     (:file "partials/menu")
                                     (:file "partials/page")))
               (:module "tests"
                        :components ((:file "util-test")))
               (:file "blockparty")))
