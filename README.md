# context-transient.el
Easily create context-specific transient menus for Emacs. Context can be anything — buffer name, current git repo, current project etc. See examples.

This is a variation on my [repo-hydra.el](https://github.com/licht1stein/repo-hydra.el) library. If you have Emacs 29.1 or later transient is built-in, so there are no external dependencies.

## Examples
Context transients are defined using `context-transient-define`. 

### Git repo context
This example defines a transient menu for git repos. Not the usage of `context-transient-repo` function — this is a helper function that returns `t` if the repo name is equal to it's argument. But context accepts any expression that evaluates to `t` or `nil`.
```elisp
(context-transient-define context-transient-repo
  :doc "Repo specific transient"
  :context (context-transient-repo "context-transient.el")
  :menu
  ["Section"
  ["Subsection"
   ("i" "Increase font" text-scale-increase :transient nil)
   ("o" "Decrease font" text-scale-decrease :transient nil)]])
  ```
 ![](./img/example-1.png)

### Buffer name context
The following example runs the transient if current buffer name is `*scratch*`:
```elisp
(context-transient-define itch
  :doc "Itch a *scratch*"
  :context (equal (buffer-name) "*scratch*")
  :menu
  [["Test" ("i" "Itch *scratch*" (lambda () (interactive) (message "Itched")))]])
```

### Clojure Specific Example
You need to be a bit more verbose to use it to run interactive CIDER commands while working on a Clojure project:
```elisp
(context-transient-define my-clj-transient
  :doc "Transient for my-clj-repo"
  :context (context-transient-repo "my-clj-repo"
  :menu 
  [["REPL"
   ("c" "Connect REPL" (lambda () (interactive) (cider-connect-clj '(:host "localhost" :port 63000))) :transient nil)
   ("d" "Sync deps" (lambda () (interactive) (cider-interactive-eval "(sync-deps)")))]
  ["Debug"
   ("p" "Start portal" (lambda () (interactive) (cider-interactive-eval "(user/portal)")))
   ("P" "Clear portal" (lambda () (interactive) (cider-interactive-eval "(user/portal-clear)")))
   ("S" "Require snitch" (lambda () (interactive) (cider-interactive-eval "(require '[snitch.core :refer [defn* defmethod* *fn *let]])")))]
  ["Systems"
   ("a" "(Re)start main system" (lambda () (interactive) (cider-interactive-eval "(user/restart-sync)")))
   ("A" "Stop main system" (lambda () (interactive) (cider-interactive-eval "(user/restart-sync)")))]]))
 ```
![](./img/example-2.png)

## Installing
Using use-package and elpaca:
```elisp
(use-package context-transient
  :elpaca (:type git :host github :repo "licht1stein/context-transient.el")
  :defer nil
  :bind ("<f6>" . context-transient))
```
I recommend binding `context-transient` to something easily accessible, F6 in the example above.
