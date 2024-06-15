(in-package :cl-repl)

(defvar *keymap*)
(defvar *keymaps* (make-hash-table :test 'equal))
(defvar *rl-default-keymap*)
(defvar *keymap-definitions* (make-hash-table :test 'equal))

(defmacro define-keymap-definition (name (&optional parent) &body bindings)
  (check-type name string)
  (let ((keymap (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,name *keymap-definitions*)
             '(let ((,keymap
                      (rl:copy-keymap (or (find-keymap ,parent)
                                          *rl-default-keymap*))))
                (loop :for (key func) :in ',bindings
                      :when (stringp key)
                        :do (rl:bind-keyseq key (eval func) :keymap ,keymap)
                      :when (characterp key)
                        :do (rl:bind-key key (eval func) :keymap ,keymap))
                (setf (gethash ,name *keymaps*) ,keymap))))))

(defun make-keymap (name)
  (or (eval (gethash name *keymap-definitions*))
      (error "CL-REPL does not know how to make keymap '~a'.~%Define one using DEFINE-KEYMAP-DEFINITION."
             name)))

(defun unbind-key (args key)
  (declare (ignore args key)))

(defun find-keymap (name)
  (gethash name *keymaps*))

(defun %cls-magic (&rest args)
  (declare (ignore args))
  (invoke-magic "%cls"))

(defun set-keymap (name)
  (let ((keymap (find-keymap name)))
    (unless (null keymap)
      (setf *keymap* name)
      (rl:set-keymap keymap))))
