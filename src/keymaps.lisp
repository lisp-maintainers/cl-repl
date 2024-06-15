(in-package :cl-repl)

(define-keymap-definition "default" ()
  ("\\C-r" #'unbind-key)
  ("\\C-s" #'unbind-key)
  ("\\C-l" #'%cls-magic))

(define-keymap-definition "inspector" ()
  (#\q #'inspector-quit)
  (#\e #'inspector-quit)
  (#\u #'inspector-move-to-previous)
  (#\0 #'inspector-select)
  (#\1 #'inspector-select)
  (#\2 #'inspector-select)
  (#\3 #'inspector-select)
  (#\4 #'inspector-select)
  (#\5 #'inspector-select)
  (#\6 #'inspector-select)
  (#\7 #'inspector-select)
  (#\8 #'inspector-select)
  (#\9 #'inspector-select)
  (#\RETURN #'unbind-key))

(define-keymap-definition "debugger" ()
  ("\\C-r" #'select-restart-by-number)
  ("\\C-o" #'step-out)
  ("\\C-x" #'step-next)
  ("\\C-s" #'step-into)
  ("\\C-t" #'show-backtrace))

(defun set-keymap (name)
  (let ((keymap (find-keymap name)))
    (unless (null keymap)
      (setf *keymap* name)
      (rl:set-keymap keymap))))

(defun make-keymaps ()
  (setf *rl-default-keymap* (rl:get-keymap))
  (make-keymap "default")
  (make-keymap "inspector")
  (make-keymap "debugger"))
