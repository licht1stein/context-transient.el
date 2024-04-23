# repo-transient.el
Easily create repo-specific transient menus for Emacs.

This is a variation on my [repo-hydra.el](https://github.com/licht1stein/repo-hydra.el) library. If you have Emacs 29.1 or later transient is built-in, so there are not external dependencies.

## Example
```elisp
(repo-transient-define
 "repo-transient.el"
 ["Section"
  ["Subsection"
   ("i" "Increase font" text-scale-increase :transient nil)
   ("o" "Decrease font" text-scale-decrease :transient nil)]])
 ```

Calling `repo-transient` anywhere form within the repo:

![](./img/example-1.png)
