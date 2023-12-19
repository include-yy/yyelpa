;;; asmd.el --- add, sub, mul, div functions -*- lexical-binding:t; -*-

;; Copyright (C) 2023 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Created: 7 Dec 2023

;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: math
;; URL: https://github.com/include-yy/notes

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; basic math functions for add, sub, mul and div operations.

;; Usage

;; (asmd-add 1 2) => 3
;; (asmd-sub 1 2) => -1
;; (asmd-mul 1 2) => 2
;; (asmd-div 1 2) => 0

;;; Code:

(defun asmd-add (a b) (+ a b))
(defun asmd-sub (a b) (- a b))
(defun asmd-mul (a b) (* a b))
(defun asmd-div (a b) (/ a b))

(provide 'asmd)
;;; asmd.el ends here
