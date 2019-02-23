;;; org-anki-backend.el --- org-anki backend         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Mikhail Pontus

;; Author: Mikhail Pontus <m.pontus@gmail.com>
;; Package-Requires: ((emacs "25") (request "0.3.0"))

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

;; Synchronizes deck with anki server.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)

(defcustom org-anki-connect-address  "127.0.0.1"
  "Listening address for anki-connect addon.")

(defcustom org-anki-connect-port
  "8765"
  "Listening port for anki-connect addon.")

(defun org-anki-request (action &optional params)
  "Send request to anki-connect server.

ACTION and PARAMS are used to construct request body per
anki-connect documentation."
  (let ((request-url (format "http://%s:%s"
			     org-anki-connect-address
			     org-anki-connect-port))
	(request-body (append `(:action ,action :version 6)
			      (and params `(:params ,params))))
	response error)
    (request request-url
	     :type "POST"
	     :data (encode-coding-string (json-encode request-body) 'utf-8)
	     :parser 'json-read
             :success (cl-function (lambda (&key data &allow-other-keys)
				     (setq response data)))
             :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
				   (setq error (string-trim (cdr error-thrown)))))
             :sync t)
    (when (or error (alist-get 'error response))
      (error (or error (alist-get 'error response))))
    (alist-get 'result response)))

(defun org-anki-backend (deck)
  "Synchronize deck representation with the anki server.

DECK describes the anki deck."
  (cl-destructuring-bind (&key id name notes) deck
    (unless id
      (setq id (number-to-string (org-anki-request "createDeck" `(:deck ,name))))
      (plist-put deck :id id))

    (dolist (note notes)
      (cl-destructuring-bind (&key id model front back tags) note
	(let ((fields `(:Front ,front :Back ,back)))
	  (if id
	      (org-anki-request "updateNoteFields"
				`(:note (:id ,id :modelName ,model :fields ,fields :tags ,tags)))
	    (setq id
		  (number-to-string
		   (org-anki-request "addNote"
				     `(:note (:deckName ,name :modelName ,model :fields ,fields :tags ,tags))							)))
	    (plist-put note :id id)))))
    deck))

(provide 'org-anki-backend)
;;; org-anki-backend.el ends here

