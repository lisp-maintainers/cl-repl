(in-package :cl-repl)

(defvar *history* nil)
(defvar *history-filename*)
(defvar *max-history-length* 1000
  "Maximum number of history entries.")
(defvar *input-relative-index* 0
  "Which previous entry is the current input.")
(defvar *edit-buffer* nil)

(defun add-history (string)
  (when string
    (setf *history* (cons string *history*))))

(defun load-history ()
  (let ((*read-eval* nil)
        (history nil)
        (filename *history-filename*))
    (when (probe-file filename)
      (with-open-file (f filename)
        (loop :while (listen f)
              :for entry := (read f nil)
              :do (when entry
                    (setf history (cons entry history))))))
    history))

(defun save-history ()
  (let* ((history *history*)
         (all-history (reverse
                       (alexandria-2:subseq* history 0 *max-history-length*)))
         (all-history (remove-if #'null all-history)))
    (with-open-file (f *history-filename*
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (loop :for input :in all-history
            :do (write input :stream f)
                (terpri f))
      (finish-output f))))

(defun reset-input ()
  (setf *input-relative-index* 0))

(defun previous-input (count key)
  (declare (ignore key))
  (let ((old-text rl:*line-buffer*)
        (new-text (nth (+ *input-relative-index* count -1)
                       *history*)))
    (when new-text
      (when (and (< 0 (length old-text))
                 (zerop *input-relative-index*))
        (setf *edit-buffer* old-text))
      (incf *input-relative-index* count)
      (setf rl:*point* (length (highlight-text rl:+prompt+)))
      (rl:replace-line new-text 0)
      (rl:redisplay)))
  0)

(defun next-input (count key)
  (declare (ignore key))
  (cond ((< 0 (- *input-relative-index* count))
         (let ((old-text rl:*line-buffer*)
               (new-text (nth (- *input-relative-index* count 1)
                              *history*)))
           (declare (ignore old-text))
           (when (and new-text
                      (not (zerop *input-relative-index*)))
             (setf rl:*point* 0)
             (decf *input-relative-index* count)
             (rl:replace-line new-text 0)
             (rl:redisplay))))
        (t
         (setf *input-relative-index* 0)
         (rl:replace-line (or *edit-buffer* "") 0)))
  0)
