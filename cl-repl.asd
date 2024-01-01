(defsystem cl-repl
  :version "0.6.6"
  :author "TANI Kojiro"
  :maintainer "Shubhamkar Ayare (digikar, shubhamayare@yahoo.co.in)"
  :license "GPLv3"
  :depends-on (#:uiop
               #:unix-opts
               #:conium
               #:cl-ppcre
               #:cl-readline)
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
