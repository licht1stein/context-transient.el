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
;; URL: https://github.com/licht1stein/context-transient.el
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

(defgroup context-transient nil "Contextual transient menus."
  :group 'convenience)

(defcustom context-transient-hook nil
  "Hook run with `context-transient'.
Each element must be a function which returns nil (to be skipped),
or a symbol naming a `context-transient' menu.
Functions are run in order until the first non-nil result is returned."
  :type 'hook)

(defun context-transient--check-repo (repo-name)
  "Check if current repo name is REPO-NAME."
  (when repo-name
    (let* ((repo-dir (locate-dominating-file "." ".git")))
      (when-let* ((repo-dir (car (last (butlast (file-name-split repo-dir))))))
        (equal repo-dir repo-name)))))

(defun context-transient--check-buffer (buff)
  "Check if current buffer name is BUFF."
  (when buff
    (equal (buffer-name) buff)))

;;@FIX: ensure returned transient is, in fact, a transient before running
(defun context-transient ()
  "Run `context-transient-hook' until success."
  (interactive)
  (if-let ((transient (run-hook-with-args-until-success 'context-transient-hook)))
      (funcall transient)
    (user-error "No transient found for current context")))


;;@FIX: Don't eval NAME arg twice.
;;@FIX: Use named functions for hook.
;;@MAYBE: Namespace defined menus.
;;@MAYBE: Don't require keyword arg for menu definition.
(cl-defmacro context-transient-define (name &key doc context menu repo buffer)
  "Define a transient MENU with NAME with DOC and DEFINITION to run in CONTEXT."
  (declare (indent 1))
  (let ((count (length (remove nil (list context repo buffer)))))
    ;; Check if the count is not exactly one
    (when (/= count 1)
      (user-error "Exactly one of :context, :repo, or :buffer must be provided")))
  `(progn (transient-define-prefix ,name () ,doc ,menu)
    (add-hook 'context-transient-hook
     (lambda ()
       (cond
        (,(macroexp-progn (list context)) ',name)
        (,(context-transient--check-buffer buffer) ',name)
        (,(context-transient--check-repo repo) ',name))))))

;; (context-transient-define context-transient-repo
;;   :doc "Repo specific transient"
;;   :repo "*scratch*"
;;   :menu
;;   [["Test" ("b" "This is repo context" (lambda () (interactive) (message "Repo context!")))]])

(provide 'repo-transient)
;;; repo-transient.el ends here
