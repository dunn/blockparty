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

(defun make-blocktable (user-id)
  "Create a table for a user's blocks."
  (pomo:query
   ;; Columns:
   ;; - the ID of the blocked user
   ;; - the reason for the block
   ;; - the UNIX time the block was registered
   "create table if not exists $1 (id bigint PRIMARY KEY, reason text DEFAULT '', time bigint);"
   (concatenate 'string "blocks_" user-id)))

(defun make-blockentry (&key user-id block-id reason)
  "Add a block to USER-ID's block table, blocking BLOCK-ID with the supplied REASON."
  (pomo:query "insert into $1 values ($2 '$3' $4);"
              (concatenate 'string "blocks_" user-id)
              block-id
              reason
              (get-universal-time)))

(defun make-jobtable (user-id)
  ;; Create a table for a user's background jobs
  (pomo:query
   ;; Columns:
   ;; - the job ID
   ;; - the instructions (whether to block or unblock, which users, etc.)
   ;; - the list of users blocked or unblocked by the job
   "create table if not exists $1 (id serial PRIMARY KEY, instructions jsonb DEFAULT '', matches text[]);"
   (concatenate 'string "jobs_" user-id)))

(defun make-jobentry (&key user-id instructions matches)
  "Create an entry for a background job."
  (pomo:query "insert into $1 values (DEFAULT $2 $3);"
              (concatenate 'string "jobs_" user-id)
              instructions
              matches))
