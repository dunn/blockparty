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

(defun html/flash (flash)
  "A <div> that's displayed temporarily at the top of a page,
usually a warning or error."
  (concatenate 'string
               "<div class='" (cdr (assoc :mode flash))
               " flash'><p>" (cdr (assoc :message flash)) "</p></div>"))

(defun html/intro (title &optional class)
  "The HTML for a page, up to the opening <body> tag."
  (concatenate 'string
               "<!DOCTYPE html><html class='"class"'><head><meta charset='utf-8'>"
               "<meta name='viewport' content='width=device-width, initial-scale=1.0, user-scalable=yes'>"
               "<link href='/stylesheets/app.css' rel='stylesheet' type='text/css' />"
               "<title>"title"</title></head><body>"))

(defun html/outro ()
  "The HTML for a page, from the deferred JavaScript to the end."
  "<!--[if lt IE 9]><script async defer src='//cdnjs.cloudflare.com/ajax/libs/html5shiv/3.7.3/html5shiv-printshiv.min.js'></script><![endif]--></body></html>")

(defun html/page (title content &optional class flash)
  "An entire HTML page."
  (concatenate 'string
               (html/intro title class)
               (if flash (html/flash flash))
               "<main>" content "</main>"
               (html/outro)))
