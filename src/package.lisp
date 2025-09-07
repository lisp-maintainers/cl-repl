(in-package :cl-user)
(defpackage :cl-repl
  (:use :cl)
  (:export main
           define-magic
           message-from-magic
           define-color-scheme
           color-scheme
           enable-syntax
           disable-syntax
           disable-debugger
           enable-debugger
           *pager-command*
           *pager-minimum-line-count*
           *pager-flush-screen*
           *default-prompt-function*
           *debugger-prompt-function*
           *output-indicator-function*
           *debugger-level*
           *read-function*
           *repl-flush-screen*
           *debugger-flush-screen*
           *inspector-flush-screen*))

(defpackage :repl-user
  (:use :cl :cl-repl))
