(in-package :cl-repl)

(defvar *history* nil)
(defvar *history-filename*)
(defvar *max-history-length* 1000
  "Maximum number of history entries.")
(defvar *input-relative-index* 0
  "Which previous entry is the current input.")

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
              :do (setf history (cons (read f) history)))))
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

(defun highlight-new-input (old-text new-text)
  (declare (ignorable old-text))
  ;; See REDISPLAY-WITH-HIGHLIGHT for an explanation of ansi sequences

  (let* ((hl-text (highlight-text new-text)))
    (loop :for line-num :from 0
          :with nl-pos := -1
          :with old-nl-pos := nil
          :while nl-pos
          :do (setq old-nl-pos (1+ nl-pos))
              (setq nl-pos (position #\newline hl-text
                                     :start (1+ nl-pos)))
              (let ((line (subseq hl-text
                                  old-nl-pos
                                  nl-pos)))
                (if (zerop line-num)
                    (format t "~c[2K~c[0G~a~a"
                            #\esc
                            #\esc
                            rl:*display-prompt*
                            line)
                    (format t "~c[2K~c[0G~a"
                            #\esc
                            #\esc
                            line)))
              (when (or nl-pos
                        (zerop line-num))
                (format t "~c[1E" #\esc)) ; move cursor down 1 line
          :finally (format t "~c[~aF~c[~aC"
                           ;; move cursor ~a lines up and ~a columns to the right
                           #\esc
                           (count #\newline new-text)
                           #\esc
                           (length (prompt-string)))))
  (rl:redisplay)
  (finish-output))


(defun previous-input (count key)
  (declare (ignore key))
  (let ((old-text rl:*line-buffer*)
        (new-text (nth (+ *input-relative-index* count -1)
                       *history*)))
    (when new-text
      (incf *input-relative-index* count)
      (setf rl:*point* 0)
      (rl:replace-line new-text 0)
      (highlight-new-input old-text new-text)))
  0)

(defun next-input (count key)
  (declare (ignore key))
  (cond ((< 0 (- *input-relative-index* count))
         (let ((old-text rl:*line-buffer*)
               (new-text (nth (- *input-relative-index* count 1)
                              *history*)))
           (when (and new-text
                      (not (zerop *input-relative-index*)))
             (setf rl:*point* 0)
             (decf *input-relative-index* count)
             (rl:replace-line new-text 0)
             (highlight-new-input old-text new-text))))
        (t
         (setf *input-relative-index* 0)
         (rl:replace-line "" 0)))
  0)
