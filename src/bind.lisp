(in-package :cl-repl)

(defun may-be-insert-newline (arg1 key)
  ;; The C library readline calling convention requires these two args
  "If the previous character is a newline, then accept-line by rl_newline
otherwise insert the newline."
  (if (or (and (= rl:*point* (length rl:*line-buffer*))
               (not (line-continue-p rl:*line-buffer*)))
          *exiting-p*)
      (cffi:foreign-funcall "rl_newline"
                            :int arg1
                            rl::int-char key
                            :int)
      (cffi:foreign-funcall "rl_insert_text"
                            :string (string #\newline)
                            :int)))

(defun previous-line (count key)
  "Move cursor to the previous line."
  (declare (ignore count key))
  (let* ((current-input rl:*line-buffer*)
         (point rl:*point*)
         (current-newline-pos
           (position #\newline current-input :end point :from-end t))
         (column (when current-newline-pos
                   (- point current-newline-pos)))

         (previous-newline-pos
           (or (when current-newline-pos
                 (position #\newline current-input
                           :end current-newline-pos :from-end t))
               0)))

    (when (and column previous-newline-pos)
      (if (< (+ previous-newline-pos column)
             current-newline-pos)
          (setf rl:*point* (+ previous-newline-pos column))
          (setf rl:*point* current-newline-pos)))))

(defun next-line (count key)
  "Move cursor to the next line."
  (declare (ignore count key))
  (let* ((current-input rl:*line-buffer*)
         (len (length current-input))
         (point rl:*point*)
         (current-newline-pos
           (position #\newline current-input :end point :from-end t))
         (column (if current-newline-pos
                     (- point current-newline-pos 1)
                     point))

         (next-newline-pos
           (if (< point len)
               (or (position #\newline current-input
                             :start (1+ point) :from-end nil)
                   point)
               point)))

    (if (< (+ next-newline-pos column 1)
           len)
        (setf rl:*point* (+ next-newline-pos column 1))
        (setf rl:*point* len))))

(defun previous-line-or-input (count key)
  "Move cursor to the previous line.
If it is already at the 0th line, move to the previous input."
  (with-cursor-information (line col)
    (if (zerop line)
        (previous-input count key)
        (previous-line count key))))

(defun next-line-or-input (count key)
  "Move cursor to the next line.
If it is already at the last line, move to the next input."
  (with-cursor-information (line col)
    (if (= line (count #\newline rl:*line-buffer*))
        (next-input count key)
        (next-line count key))))

(defun bind-multiline-keys ()
  (rl:bind-key #\return  #'may-be-insert-newline)
  (rl:bind-key #\newline #'may-be-insert-newline)

  (rl:bind-keyseq (format nil "~c[A" #\esc) #'previous-line-or-input) ; Up
  (rl:bind-keyseq (format nil "~c[B" #\esc) #'next-line-or-input)     ; Down
  (rl:bind-keyseq (format nil "~c" #\dle) #'previous-line-or-input)   ; C-p
  (rl:bind-keyseq (format nil "~c" #\so)  #'next-line-or-input)       ; C-n

  (rl:bind-keyseq (format nil "~c[1;5A" #\esc) #'previous-input) ; Ctrl+Up
  (rl:bind-keyseq (format nil "~c[1;5B" #\esc) #'next-input)     ; Ctrl+Down
  (rl:bind-keyseq (format nil "~cp" #\esc) #'previous-input)     ; M-p
  (rl:bind-keyseq (format nil "~cn" #\esc) #'next-input)         ; M-n

  (setf rl:*blink-matching-paren* t)
  (cffi:foreign-funcall "rl_bind_key"
                        :char 41        ; )
                        :pointer (cffi:foreign-symbol-pointer "rl_insert_close")
                        :pointer *rl-default-keymap*))
