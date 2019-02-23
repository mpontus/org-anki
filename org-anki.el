;;; org-anki.el --- Export notes stored in ORG files as Anki decks  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Mikhail Pontus

;; Author: Mikhail Pontus(require 'org-anki-frontend) <michael@Mac-mini-Mikhail.local>
;; Keywords: 

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

;; 

;;; Code:

(require 'org-anki-frontend)
(require 'org-anki-backend)

(defun org-anki-sync ()
  (interactive)
  (org-anki-frontend 'org-anki-backend))

(provide 'org-anki)
;;; org-anki.el ends here
