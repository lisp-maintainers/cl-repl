(in-package :cl-repl)

(defconstant +version+
  (intern (asdf:component-version (asdf:find-system "cl-repl"))))

(defvar *logo*
  "  ___  __          ____  ____  ____  __
 / __)(  )    ___ (  _ \\(  __)(  _ \\(  )
( (__ / (_/\\ (___) )   / ) _)  ) __// (_/\\
 \\___)\\____/      (__\\_)(____)(__)  \\____/
")

(defvar *copy* "(C) 2017-2018 TANI Kojiro <kojiro0531@gmail.com>")
(defvar *maintain* "Maintained by: Lisp Maintainers
Github: https://github.com/lisp-maintainers/cl-repl")

(defvar *versions*
  (format nil "cl-repl ~a on ~?~a ~a"
          +version+
          #+ros.script
          "Roswell ~a, "
          #-ros.script
          ""
          #+ros.script
          `(,(ros::version))
          #-ros.script
          nil
          (lisp-implementation-type)
          (lisp-implementation-version)))

(defvar *site-init-path* #P"~/.replrc")
(defun site-init ()
  (unless (probe-file *site-init-path*)
    (return-from site-init))
  (handler-case (load *site-init-path*)
    (error (c)
      (progn
        (format *error-output* "Failed to load ~a, quitting.~%[~a]~%" *site-init-path* c)
        (uiop:quit 1)))))

(defparameter *repl-flush-screen* nil)

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(opts:define-opts
  (:name :help
   :description "Print this help and exit."
   :short #\h
   :long "help")
  (:name :version
   :description "Show the version info and exit."
   :short #\v
   :long "version")
  (:name :no-init
   :description "Skip to load init file."
   :short #\n
   :long "no-init")
  (:name :history-file
   :description "Specifies which history file to use. If unspecified, this is the .cl-repl file in $HOME directory."
   :long "history-file"
   :arg-parser #'identity)
  (:name :load
   :description "Load a file"
   :short #\l
   :long "load"
   :arg-parser #'identity)
  (:name :eval
   :description "Eval a form"
   :short #\e
   :long "eval"
   :arg-parser #'identity)
  (:name :disable-debugger
   :description "Disable debugger: print error without dropping into the debugger"
   :short #\d
   :long "disable-debugger"))

(defun main-prep ()
  (bind-multiline-keys)
  (enable-syntax)
  (rl:register-function :complete #'completer)
  (install-inspector))

(defun main (&optional (argv nil argvp) &key (show-logo t))
  (main-prep)
  (let ((*debugger-enabled-p* t))
    (multiple-value-bind (options free-args)
        (handler-case
            (if argvp (opts:get-opts argv) (opts:get-opts))
          (error (e)
            (format uiop:*stderr* "~a: ~a"
                    (class-name (class-of e))
                    e)
            (uiop:print-backtrace :stream uiop:*stderr* :condition e)
            (format t "try `cl-repl --help`.~&")
            (uiop:quit 1)))
      (declare (ignore free-args))
      (when-option (options :help)
        (opts:describe
         :prefix "A full-featured Common Lisp REPL implementation.")
        (uiop:quit 0))
      (when-option (options :disable-debugger)
        (setq *debugger-enabled-p* nil))
      (when-option (options :version)
        (format t "cl-repl v~a~&" +version+)
        (uiop:quit 0))
      (when-option (options :no-init)
        (setf *site-init-path* nil))
      (setf *history-filename*
            (or (getf options :history-file)
                (format nil "~a/.cl-repl" (uiop:getenv "HOME"))))
      (when *site-init-path*
        (site-init))
      (setf *history* (load-history))
      (loop for (k v) on options by #'cddr
            do (case k
                 (:eval (eval (read-from-string v)))
                 (:load (load v)))))
    (when *repl-flush-screen* (flush-screen))
    (with-cursor-hidden
      (when show-logo
        (format t (color *logo-color* *logo* :prompt-chars nil)))
      (format t "~a~%~a~%~a~2%" *versions* *copy* *maintain*))
    (in-package :cl-user)
    (unwind-protect
         (let ((*debugger-hook* (if *debugger-enabled-p*
                                    #'debugger
                                    #'display-error-without-debugging)))
           (repl))
      (save-history)
      (rl:deprep-terminal))
    (when *repl-flush-screen* (flush-screen))))
