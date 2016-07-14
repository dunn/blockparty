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

(lisp-unit:define-test delete-session-test
  (red:set "fake_session:passwd" "fake_passwd")
  (red:set "fake_session:token" "fake_token")
  (red:set "fake_session:secret" "fake_secret")

  (lisp-unit:assert-equal "fake_passwd" (red:get "fake_session:passwd"))
  (lisp-unit:assert-equal "fake_token" (red:get "fake_session:token"))
  (lisp-unit:assert-equal "fake_secret" (red:get "fake_session:secret"))

  (delete-session "fake_session")
  (lisp-unit:assert-eq nil (red:get "fake_session:passwd"))
  (lisp-unit:assert-eq nil (red:get "fake_session:token"))
  (lisp-unit:assert-eq nil (red:get "fake_session:secret")))
