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

;;;###autoload
(defun context-transient-clear ()
  "Remove all previously defined transients from `context-transient-hook'."
  (interactive)
  (setq context-transient-hook nil)
  (message "All context transients removed."))

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
;;;###autoload
(defun context-transient ()
  "Run `context-transient-hook' until success."
  (interactive)
  (if-let ((transient (run-hook-with-args-until-success 'context-transient-hook)))
      (funcall transient)
    (user-error "No transient found for current context")))

(cl-defun context-transient--check-conditions (&key repo buffer context)
  "Check if any of the REPO, BUFFER or CONTEXT conditions are true."
  (or
   (and repo (context-transient--check-repo repo))
   (and buffer (context-transient--check-buffer buffer))
   (and context (macroexp-progn (list context)))))

(defun context-transient--generate-function-name (name)
  "Generate standardized menu function names using NAME and prefix."
  (intern (concat "context-transient/" (symbol-name name))))

;;@FIX: Don't eval NAME arg twice.
;;@MAYBE: Don't require keyword arg for menu definition.
(cl-defmacro context-transient-define (name &key doc menu context repo buffer)
  "Define a transient MENU with NAME with DOC and DEFINITION to run in CONTEXT.

The resulting transient will be called `context-transient/NAME'"
  (let ((count (length (remove nil (list context repo buffer))))
        (fn-name (context-transient--generate-function-name name))
        (transient-name (intern (concat "context-transient-menu/" (symbol-name name)))))
    ;; Check if the count is not exactly one
    (when (/= count 1)
      (user-error "Exactly one of :context, :repo, or :buffer must be provided"))
    (declare (indent 1))
    `(progn
       (transient-define-prefix ,transient-name () ,doc ,menu)
       (defun ,fn-name nil
        "Automatically generated function to check if `context-transient' conditions are currently met."
        (when
            (context-transient--check-conditions :repo ,repo :buffer ,buffer :context ,context)
          ',transient-name))
       (unless (memq ',fn-name context-transient-hook)
        (add-hook 'context-transient-hook (function ,fn-name))))))

;; (context-transient-define context-transient-repo
;;   :doc "Repo specific transient"
;;   :repo "context-transient.el"
;;   :menu
;;   [["Test" ("b" "This is repo context" (lambda () (interactive) (message "Repo context!")))]])

(provide 'repo-transient)
;;; repo-transient.el ends here
