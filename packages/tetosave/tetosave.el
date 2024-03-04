;;; tetosave.el --- Auto save files when idle -*- lexical-binding:t; -*-

;; Copyright (C) 2013 ~ 2014, Andy Stewart, all rights reserved.

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Created: 2013-12-31 00:32:00
;; ORG-URL: https://github.com/manateelazycat/auto-save
;; Maintainer: include-yy <yy@egh0bww1.com>

;; Package-Version: 1.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/include-yy/tetosave

;; This file is NOT part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Auto save file when emacs idle

;; Installation
;; 1) Clone or download this repository
;; 2) In your `~/.emacs' or `init.el', add the following three lines:
;;    (add-to-list 'load-path "<path-to-tetosave>")
;;    (require 'tetosave)
;;    (tetosave-enable)

;; Customization
;; OPTION `tetosave-idle' determines how much time should elapse
;; before triggering autosave when Emacs enters idle state
;; OPTION `tetosave-silent' determines whether Emacs displays a
;; message in the echo area when saving files
;; OPTION `tetosave-delete-trailing-whitespace' determines whether to
;; delete excess whitespace characters during autosave

;; If you don't want some buffers to be autosaved, you can add
;; predicates in `tetosave-disable-predicates'. For example, if
;; you don't want it in `authinfo-mode', you can do it like this:
;; (add-to-list 'auto-save-disable-predicates
;;	        (lambda () (eq major-mode 'authinfo-mode)))

;; That's all, Enjoy it!  ~Thanks LazyCat for such a handy package~

;;; Code:

(defgroup tetosave nil
  "Auto save file when emacs idle."
  :group 'tetosave)

(defcustom tetosave-idle 1.0
  "The idle seconds to auto save file."
  :type 'number)

(defcustom tetosave-silent t
  "If non-nil, don't show any save message in echo area"
  :type 'boolean)

(defcustom tetosave-delete-trailing-whitespace t
  "If non-nil, delete trailing whitespaces except current line when saving."
  :type 'boolean)

(defvar tetosave-disable-predicates nil
  "disable auto save in these case.
Each predicate has zero argument and is executed in the context
of the buffer environment.
If any one of the functions returns true, then that buffer should
not be saved.")

(defvar tetosave-timer nil
  "Timer used for saving the buffer")

(defun tetosave-buffer-savable-p ()
  "Determine if the buffer can be saved, t for True, otherwise False"
    (and
     ;; is buffer associates with a file?
     (buffer-file-name)
     ;; is buffer modified?
     (buffer-modified-p)
     ;; is allow by all predicates?
     (not (seq-some (lambda (p) (funcall p))
		    tetosave-disable-predicates))))

(defun tetosave-log (buflist)
  "Output information related to the saved buffer."
  (cond
   ;; It's stupid tell user if nothing to save.
   ((= (length buflist) 1)
    (message "# Saved %s" (car buflist)))
   ((> (length buflist) 1)
    (message "# Saved %d files: %s"
             (length buflist)
             (mapconcat 'identity buflist ", ")))))

(defun tetosave-save-buffer ()
  "save function used for `tetosave-save-buffers'.
set `write-region-inhibit-fsync' to `t' can speed up save operation."
  (let ((write-region-inhibit-fsync t))
    (basic-save-buffer)))

(defun tetosave-delete-whitespace ()
  "remove all excess spaces and trailing blank lines except for the current line.
internally uses `delete-trailing-whitespace'"
  (interactive)
  (let ((buffer-read-only nil)
	(begin (line-beginning-position))
        (end (point)))
    (save-excursion
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)))
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region end (point-max))
          (delete-trailing-whitespace))))))

(defun tetosave-save-buffers ()
  (let ((buflist))
    ;; get all saveable buffers from `buffer-list'
    (save-current-buffer
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(when (tetosave-buffer-savable-p)
          (push (buffer-name buf) buflist)
	  ;; delete whitespaces
	  (when tetosave-delete-trailing-whitespace
	    (tetosave-delete-whitespace))
	  (if tetosave-silent
	      ;; `inhibit-message' can shut up Emacs, but we want
	      ;; it doesn't clean up echo area during saving
	      (with-temp-message (current-message)
		(let (;; `inhibit-message' make save message don't show in
		      ;; minibuffer
		      (inhibit-message t))
		  (tetosave-save-buffer)))
            (tetosave-save-buffer)))))
    ;; Show save log message
    (unless tetosave-silent
      (tetosave-log buflist))))

(defun tetosave-disable ()
  "disalbe tetosave"
  (interactive)
  (when tetosave-timer
    (cancel-timer tetosave-timer)
    (setq tetosave-timer nil)))

;;;###autoload
(defun tetosave-enable ()
  "enable tetosave"
  (interactive)
  (tetosave-disable)
  (setq tetosave-timer
	(run-with-idle-timer
	 tetosave-idle t
	 'tetosave-save-buffers)))

;; additional default disable predicators
(defun tetosave-pred-org-capture ()
  "predicator for org-capture's buffer and indirect buffer

Due to the use of an indirect buffer by org-capture to record a
new capture, even if we check for org-capture-mode, its
base-buffer will still be saved because of the lack of
inspection.
Therefore, we can simply check whether a specific org file has an
indirect buffer. Since the buffer-file-name of an indirect buffer
is nil, there is no need to inspect it."
  (eq (buffer-base-buffer
       (get-buffer (concat "CAPTURE-" (buffer-name))))
      (current-buffer)))

(defun tetosave-pred-corfu ()
  "predicator for active corfu completion"
  (and (boundp 'corfu--total)
       (not (zerop corfu--total))))

(defun tetosave-pred-company ()
  "predicator for active company completion"
  (bound-and-true-p company-candidates))

;; add default predicators to `tetosave-disable-predicates'
(dolist (a '(tetosave-pred-org-capture
	     tetosave-pred-corfu
	     tetosave-pred-company))
  (add-to-list 'tetosave-disable-predicates a))

(provide 'tetosave)
;;; tetosave.el ends here
