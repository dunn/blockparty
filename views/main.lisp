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

(in-package #:blockparty)

(defun view/main (screen-name &optional class flash)
  "The main page (when you're logged in)."
  (page/full
   "Block Party"
   (concatenate 'string
                "<h1>Hi @" screen-name "</h1>"
                "<p>You're probably here because you need to "
                "block or mute some assholes on Twitter dot com.  "
                "Create a filter with the form below and get going.</p>"
                (form/filters))
   class flash))
