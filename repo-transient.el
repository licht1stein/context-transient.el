;;; repo-transient.el --- Git specific transients -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; Created: 23 Apr 2024
;;
;; URL: https://github.com/licht1stein
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Easily define and call repo-specific transient menus.
;;; Code:
(require 'transient)

(defvar repo-transient--transients-map
  (make-hash-table :test #'equal)
  "Hash-map with all transient maps.")

(defun repo-transient--current-git-repo ()
  "Return current repository root foler name as a string."
  (let* ((repo-dir (locate-dominating-file "." ".git")))
    (when repo-dir
      (car (last (butlast (file-name-split repo-dir)))))))

;;;###autoload
(defmacro repo-transient-define (repo-name &rest menu-entries)
  "Create a repository specific transient menu.

REPO-NAME - must be the same as repository root directory name
MENU-ENTRIES - `transient-define-prefix' args"
  (let* ((tr-cmd-name (read (concat "repo-transient-" (downcase repo-name))))
         (docstring (format "Repo transient for %s" repo-name)))
    (puthash repo-name tr-cmd-name repo-transient--transients-map)
	  `(progn
	     (transient-define-prefix ,tr-cmd-name ()
		     ,docstring
		     ,@menu-entries))))

;;;###autoload
(defun repo-transient ()
  "Launch current repo's transient if defined."
  (interactive)
  (let* ((repo (repo-transient--current-git-repo))
         (menu-name (gethash repo repo-transient--transients-map)))
    (cond ((not repo)  (error "Error: not in a git repo"))
          (menu-name (funcall menu-name))
          (t (error "No repo transient defined for %s" repo)))))

(provide 'repo-transient)
;;; repo-transient.el ends here
