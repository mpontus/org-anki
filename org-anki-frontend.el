;;; org-anki-frontend.el --- org anki frontend       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Mikhail Pontus

;; Author: Michael Pontus <m.pontus@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Responsible for extracting deck representation from org file and
;; actualizing it after synchronization with the server.

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-html)

(defun org-anki-skip-property-block ()
  "Move cursor to the end of the property block if one exists."
  (when (looking-at org-property-drawer-re)
    (goto-char (cdr (org-get-property-block))) 
    (forward-line)))

(defun org-anki-get-body ()
  "Export subtree body as HTML."
  (save-restriction
    ;; Narrow to subtree contents excluding headline and property block
    (org-narrow-to-subtree)
    (forward-line)
    (org-anki-skip-property-block)
    (narrow-to-region (point) (point-max))
    ;; Export visible contents as html
    (let ((org-export-with-toc nil))
      ;; Narrow to the body excluding headline
      (org-export-as 'html nil nil t nil))))

(defmacro org-anki-map-subheadings (&rest body)
  "Execute BODY with cursor pointing at every direct subheading."
  (declare (debug (body)) (indent 1))
  `(let (results)
     (save-restriction
       (org-narrow-to-subtree)
       (when (outline-next-heading)
	 (push (save-excursion ,@body) results)
	 (while (outline-get-next-sibling)
	   (push (save-excursion ,@body) results))))
     (nreverse results)))

(defun org-anki-frontend/read ()
  "Read deck entries from org subtree."
  (save-excursion
    `(:id ,(org-entry-get (point) "ANKI_DECK_ID")
	  :name ,(org-get-heading)
	  :notes ,(org-anki-map-subheadings 
		   `(:id ,(org-entry-get (point) "ANKI_NOTE_ID")
			 :model "Basic"
			 :front ,(org-get-heading)
			 :back ,(org-anki-get-body)
			 :tags [])))))

(defun org-anki-frontend/write (deck)
  "Update org file with updated DECK values."
  (save-excursion
    (cl-destructuring-bind (&key id name notes) deck
      (org-entry-put (point) "ANKI_DECK_ID" id)
      (org-anki-map-subheadings
       (org-entry-put (point) "ANKI_NOTE_ID" (plist-get (pop notes) :id))))))

(defun org-anki-frontend (cb)
  "Default frontend for org-anki.

Calls CB with deck representation extracted from subtree under
cursor, and updates the subtree with the returned results."
  (org-anki-frontend/write
   (funcall cb (org-anki-frontend/read))))


(provide 'org-anki-frontend)
;;; org-anki-frontend.el ends here
