;;; doc-view-save-slice.el --- Persistent slice setting per doc-view file

;; Copyright (C) 2018 Marcin Kolenda

;; Author: Marcin Kolenda <marcinkolenda419@gmail.com>
;; Maintainer: Marcin Kolenda <marcinkolenda419@gmail.com>
;; Version: 1.0
;; Package-Requires: (doc-view)
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Persistent slice setting per doc-view file.

;; Commands:
;;
;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; Installation:

;; Add the following to your emacs init file:
;;
;; (require 'doc-view-save-slice)
;; (add-hook 'doc-view-mode-hook 'doc-view-save-slice-mode)

;; That's all.

;;; Code:
(require 'doc-view)

(defgroup doc-view-save-slice nil
  "Automatically save slice configuration."
  :group 'data)

;; "Hash table of saved slices to go back to when revisiting files. Each element
;; looks like (FILENAME . ((LEFT_X TOP_Y WIDTH HEIGHT) . IMAGE_WIDTH .
;; RESOLUTION)); visiting file FILENAME slices automatically to (LEFT_X TOP_Y
;; WIDTH HEIGHT). This hash table is saved between Emacs sessions."
(setq doc-view-save-slice-hash (make-hash-table :test 'equal))

(defcustom doc-view-save-slice-file (locate-user-emacs-file "doc-view-slices" nil)
  "Name of the file that records `doc-view-save-slice-hash' value."
  :type 'file)


(defvar doc-view-save-slice-loaded nil
  "Non-nil means that the `doc-view-save-slice' has been loaded.")

(defun doc-view-save-slice--setup-hooks (add)
  (cond
   (add
    (unless noninteractive
      (add-hook 'kill-emacs-hook #'doc-view-save-slice-kill-emacs-hook))
    (add-hook 'kill-buffer-hook #'doc-view-save-slice-to-hash))
   (t
    ;; we may remove hooks here

    )))

(define-minor-mode doc-view-save-slice-mode
  "Non-nil means automatically save slice in each doc-view file.
This means when you visit a file, view is sliced exactly like it
was when you previously visited the same file."
  :group 'doc-view-save-slice
  (doc-view-save-slice-load-from-hash)
  (doc-view-save-slice--setup-hooks doc-view-save-slice-mode))

(make-variable-buffer-local 'doc-view-save-slice-mode)

(defun doc-view-save-slice-load-from-hash ()
  (or doc-view-save-slice-loaded (doc-view-load-slice-hash-from-file))
  (let ((cell (gethash buffer-file-name doc-view-save-slice-hash)))
    (if cell
	      (progn
          (setq-local doc-view-image-width (cadr cell))
          (setq-local doc-view-resolution (caddr cell))
          (doc-view-reconvert-doc)
          (apply 'doc-view-set-slice (car cell))
          ;; and make sure it will be saved again for later
          (setq doc-view-save-slice t)))))

(defun doc-view-save-slice-to-hash ()
  (or doc-view-save-slice-loaded (doc-view-load-slice-hash-from-file))
  (condition-case nil
   (let ((slice-conf (image-mode-window-get 'slice)))
     (when (listp slice-conf)
       (setf (gethash buffer-file-name doc-view-save-slice-hash)
             (list slice-conf doc-view-image-width doc-view-resolution))))
   (error nil)))

(defun doc-view-save-slice-hash-to-file ()
  (let ((file (expand-file-name doc-view-save-slice-file))
        (coding-system-for-write 'utf-8))
    (with-current-buffer (get-buffer-create " *Saved Slices*")
      (delete-region (point-min) (point-max))
      (insert (format ";;; -*- coding: %s -*-\n"
                      (symbol-name coding-system-for-write)))
      (let ((print-length nil)
            (print-level nil))
        (pp doc-view-save-slice-hash (current-buffer)))
      (condition-case nil
	        ;; Don't use write-file; we don't want this buffer to visit it.
          (write-region (point-min)
                        (point-max) file)
        (file-error (message "Saving slices: can't write %s" file)))
      (kill-buffer (current-buffer)))))

(defun doc-view-load-slice-hash-from-file ()
  (if (not doc-view-save-slice-loaded)
      (progn
        (setq doc-view-save-slice-loaded t)
        (let ((file (expand-file-name doc-view-save-slice-file)))
          (if (file-readable-p file)
              ;; don't want to use find-file because we have been
              ;; adding hooks to it.
              (with-current-buffer (get-buffer-create " *Saved Slices*")
                (delete-region (point-min) (point-max))
                (insert-file-contents file)
                (goto-char (point-min))
                (setq doc-view-save-slice-hash (read (buffer-string)))
                (kill-buffer (current-buffer))))
          nil))))

(defun doc-view-save-slice-kill-emacs-hook ()
  (doc-view-save-slice-to-hash)
  (if doc-view-save-slice-loaded
      (doc-view-save-slice-hash-to-file)))

(provide 'doc-view-save-slice)

;;; doc-view-save-slice.el ends here
