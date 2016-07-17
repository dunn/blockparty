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

(defun form/filters ()
  "Return an HTML form for block/mute filters."
  "<form id='filter-form' action='/block' method='post'>
    <div id='filters'>
      <label for='action'>I want to</label>
      <select name='action' id='action'>
        <option value='block'>block</option>
        <option value='unblock'>unblock</option>
        <option value='mute'>mute</option>
        <option value='unmute'>unmute</option>
        <option value='unfollow'>unfollow</option>
      </select>
      <div class='filter'>
        <label for='filter-1a'>everyone who</label>
        <select name='filter-1a' id='filter-1a'>
          <option value='follows'>follows</option>
          <option value='is-followed-by'>is followed by</option>
        </select>
        <label hidden for='filter-1b'>Twitter user</label>
        <input type='text' name='filter-1b' id='filter-1b' placeholder='@username' />
      </div>
    </div>
    <button type='button' id='add-filter' class='add-filter'>Add a filter</button>
    <input type='submit' />
  </form>"
   )
