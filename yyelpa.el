;;; yyelpa.el --- yy's ELPA management tool -*- lexical-binding:t; -*-

;; Copyright (C) 2023 include-yy <yy@egh0bww1.com>

;; Author: include-yy <yy@egh0bww1.com>
;; Created 20 Dec 2023

;; Package-Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/include-yy/yy-elpa

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

;; 用于创建 ELPA 的简单构建工具

;;; Code:

(require 'package)
(require 'package-x)
(require 'dired)

(defgroup yyelpa nil
  "include-yy's ELPA manager"
  :group 'applications
  :version "29.1")

(defcustom yyelpa-tar-executable "tar"
  "tar 命令的位置，在 Linux 系统上不用指定"
  :type 'string)

(defcustom yyelpa-build-verbose nil
  "是否在构建过程中输出细节"
  :type 'boolean)

(defvar yyelpa-dir (expand-file-name ".")
  "yyelpa 在本地的项目根目录")

(defvar yyelpa-pkg-dir (expand-file-name "packages")
  "yyelpa 的包目录")

(defvar yyelpa-build-dir (expand-file-name "build/packages")
  "包的导出目录，设置为 yyelpa.el 脚本所在目录的 build 子目录")

(defun yyelpa-dir-info (dir)
  "获取某目录中的 package 信息，信息来源必须是与目录同名的主要 el 文件
成功则返回 package-desc 结构，否则返回 nil"
  (let* ((dirname (file-name-base (directory-file-name dir)))
	 (mainfile (file-name-concat dir (concat dirname ".el")))
	 info)
    (when (file-exists-p mainfile)
      (with-temp-buffer
	(insert-file-contents mainfile)
	(setq info (ignore-errors (package-buffer-info)))))
    info))

(defun yyelpa-write-pkg-file (desc dir)
  "创建某包的 -pkg.el 文件，并写入到对应目录
要求 desc 为合理的 package-desc 结构
来自 MELPA 的 package-build.el，同样也可参考 package.el 的
`package-generate-description-file'"
  (let ((name (package-desc-name desc)))
    (with-temp-file (expand-file-name (format "%s-pkg.el" name) dir)
      (pp `(define-package ,(symbol-name name)
             ,(package-version-join (package-desc-version desc))
             ,(package-desc-summary desc)
             ',(mapcar (pcase-lambda (`(,pkg ,ver))
                         (list pkg (package-version-join ver)))
                       (package-desc-reqs desc))
             ,@(cl-mapcan (pcase-lambda (`(,key . ,val))
                            (list key val))
                          (package-desc-extras desc)))
          (current-buffer))
      (princ ";; Local Variables:\n;; no-byte-compile: t\n;; End:\n"
             (current-buffer)))))

(defun yyelpa-gen-pkg-file (dir)
  "根据包的主文件生成 pkg.el"
  (if-let ((desc (yyelpa-dir-info dir)))
      (prog1 desc
	(yyelpa-write-pkg-file desc dir))
    (error "yyelpa: %s doesn't have a valid main file" dir)))

(defun yyelpa-create-tar (desc dir)
  "在某一目录创建 tar 打包文件"
  (let* ((dirname (file-name-base (directory-file-name dir)))
	 (version (mapconcat 'number-to-string
			     (package-desc-version desc) "."))
	 (name (concat dirname "-" version))
	 (files (cons "README"
		      (directory-files dir nil (concat "^" dirname ".*" "\\.el\\'"))))
	 (tarname (concat name ".tar")))
    (let ((default-directory dir))
      (make-directory name t)
      (dolist (f files)
	(copy-file f (file-name-concat name f) t))
      (process-file
       yyelpa-tar-executable nil
       (get-buffer-create "*yyelpa-build-checkout*") nil
       "-cf" tarname name)
      (delete-directory name t)
      (when yyelpa-build-verbose
	(message "Created %s containing:" tarname)
	(dolist (line (sort (process-lines yyelpa-tar-executable
					   "--list" "--file" tarname)
			    #'string<))
	  (message "  %s" line))))))

(defun yyelpa-upload-tar (desc dir)
  "将包发布到 `yyelpa-build-dir' 所在位置
目录中需要存在与目录同名的 tar 文件"
  (let* ((package-archive-upload-base yyelpa-build-dir)
	 (dirname (file-name-base (directory-file-name dir)))
	 (version (mapconcat 'number-to-string
			     (package-desc-version desc) "."))
	 (name (concat dirname "-" version))
	 (tarname (expand-file-name (concat name ".tar") dir)))
    (if (file-exists-p tarname)
	(package-upload-file tarname)
      (error "yyelpa: %s.tar not exist, cannot upload" name))))

(defun yyelpa-update (dir)
  "对某个被选中的包进行打包构建并传至 build 目录
无缓存和修改时间检查"
  (interactive (list (completing-read
		      "Select a package: "
		      (directory-files yyelpa-pkg-dir
				       nil directory-files-no-dot-files-regexp)
		      nil t)))
  (let* ((fulldir (expand-file-name dir yyelpa-pkg-dir))
	 (desc (yyelpa-gen-pkg-file fulldir)))
    (yyelpa-create-tar desc fulldir)
    (yyelpa-upload-tar desc fulldir)))

(defun yyelpa-dired ()
  "打开 packages 所在目录的 dired buffer"
  (interactive)
  (dired yyelpa-pkg-dir))

(defun yyelpa-update-multi (names)
  "在 packages 所在目录的 dired buffer 中更新选中的 package"
  (interactive (list (if (not (and (eq major-mode 'dired-mode)
				   (string= (directory-file-name default-directory)
					    yyelpa-pkg-dir)))
			 (user-error "yyelpa: not in package dir")
		       (dired-get-marked-files t))))
  (mapc 'yyelpa-update names))

(provide 'yyelpa)
;;; yyelpa.el ends here
