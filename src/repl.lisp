(in-package :cl-repl)

(defun exit-with-prompt ()
  (finish-output)
  (when (zerop *debugger-level*)
    (let ((*exiting-p* t))
      (string-case
          (rl:readline :prompt "Do you really want to exit ([y]/n)? ")
        (nil (terpri))
        ("")
        ("y")
        ("n" (return-from exit-with-prompt (setf *last-input* "nil")))
        (otherwise (return-from exit-with-prompt (exit-with-prompt))))))
  (throw *debugger-level* nil))

(defvar *output-indicator-function*
  #'(lambda () "[OUT]: "))

(defun print-result (values)
  (reset-input)
  (format t "~&~a~{~s~^~%~}~%"
          (color *output-indicator-color*
                 (funcall *output-indicator-function*)
                 :prompt-chars nil)
          values)
  (finish-output) t) 

(defun eval-print (-)
  (format t "~&")
  (finish-output)
  (let ((values (multiple-value-list (eval -))))
    (setq +++ ++ /// // *** (car ///)
          ++ + // / ** (car //)
          + - / values * (car /))
    (print-result values)))

(defmacro with-extra-restarts (form &rest restarts)
  `(restart-case ,form
     (*abort () :report "Deduce debugger level." t)
     (*exit () :report "Exit CL-REPL." (throw 0 nil))
     ,@restarts))

(defvar *read-function* #'read-from-string
  "Function that will be used to convert the input string to a lisp expression.")

(defun read-eval-print (&key (level 0))
  (let ((*debugger-level* level))
    (with-extra-restarts
        (eval-print (setq - (funcall *read-function* (read-input))))
      (*retry () :report "Try evaluating again."
              (with-extra-restarts (eval-print -))))))

(defun repl ()
  (loop :when (string/= *keymap* "default")
              :do (set-keymap "default")
        :while (catch 0 (read-eval-print))))
