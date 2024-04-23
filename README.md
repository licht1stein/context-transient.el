# repo-transient.el
Easily create repo-specific transient menus for Emacs.

This is a variation on my [repo-hydra.el](https://github.com/licht1stein/repo-hydra.el) library. If you have Emacs 29.1 or later transient is built-in, so there are not external dependencies.

## Example
To define a repo-specific transient use `repo-transient-define` with repository name as the first argument, and the transient menu as the second argument:
```elisp
(repo-transient-define
 "repo-transient.el"
 ["Section"
  ["Subsection"
   ("i" "Increase font" text-scale-increase :transient nil)
   ("o" "Decrease font" text-scale-decrease :transient nil)]])
 ```

Calling `repo-transient` anywhere from within the repo:

![](./img/example-1.png)

## Installing
Using use-package and elpaca:
```elisp
(use-package repo-transient
  :elpaca (:type git :host github :repo "licht1stein/repo-transient.el")
  :defer nil
  :bind ("<f6>" . repo-transient))
```
I recommend binding `repo-transient` to something easily accessible, F6 in the example above.
