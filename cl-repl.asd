(asdf:defsystem cl-repl
  :version "0.7.0"
  :author "TANI Kojiro"
  :maintainer "Lisp Maintainers (https://github.com/lisp-maintainers/cl-repl)"
  :license "GPLv3"
  :build-operation "program-op"
  :build-pathname "cl-repl"
  :entry-point "cl-repl:main"
  :depends-on (#:uiop
               #:unix-opts
               #:cl-ppcre
               #:cl-readline
               #:trivial-backtrace)
  :serial t
  :components ((:module "src" :components ((:file "package")
                                           (:file "util")
                                           (:file "color")
                                           (:file "color-scheme")
                                           (:file "highlight")
                                           (:file "keymap")
                                           (:file "pager")
                                           (:file "command")
                                           (:file "shell")
                                           (:file "completer")
                                           (:file "debugger")
                                           (:file "inspector")
                                           (:file "input")
                                           (:file "history")
                                           (:file "repl")
                                           (:file "bind")
                                           (:file "main"))))
  :description "A full-featured repl implementation."
  :long-description "A full-featured repl implementation.")
