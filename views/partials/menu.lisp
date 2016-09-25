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

(defun menu (&key logged-in)
  "Return user menu depending on whether the user is LOGGED-IN."
  (concatenate
   'string
   "<ul class='menu'>"
   (if logged-in
       (concatenate
        'string
        "<li><a href='/settings' title='User preferences'>Settings</a></li>"
        "<li><a href='/history' title='View and revert block actions'>History</a></li>"
        "<li><a href='/logout' title='Log out of Block Party'>Logout</a></li>")
     "<li><a href='/login' title='Log into Block Party'>Login</a></li>")
   "</ul>"))
