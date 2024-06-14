(in-package :cl-repl)

(defun escape-name (name)
  (ppcre:regex-replace-all
    "\\+"
    (ppcre:regex-replace-all
      "\\*"
      name "\\\\*")
    "\\\\+"))

(defun list-regex (lst)
  (format nil
          "((?<=\\s)|^|(?<=\\()|(?<=\\)))(~{~a|~})(?=(\\s|\\(|\\)|$))"
          (sort lst #'string>)))

(destructuring-bind (functions specials)
  (loop :for sym :being :the :external-symbols :of :cl
        :when (handler-case (symbol-function sym) (error () nil))
              :collect (escape-name (string-downcase sym)) :into functions
        :when (special-operator-p sym)
              :collect (escape-name (string-downcase sym)) :into specials
        :finally (return (list functions specials)))
  (defvar *syntax-table*
    (list
     :magic (list *magic-syntax-color* "^%\\S+")
     :string (list *string-syntax-color* "\".*?\"")
     :variable (list *variable-syntax-color* "([\\*])\\S+\\1")
     :constant (list *constant-syntax-color* "([\\+])\\S+\\1")
     :keyword (list *keyword-syntax-color* "((?<=\\s)|^):\\S+(?=\\b)")
     :definition (list *definition-syntax-color*
                       "((?<=defun)|(?<=defmacro)|(?<=defmethod)|(?<=defgeneric))\\s\\S+(?=\\b)")
     :lambda (list *lambda-syntax-color*
                   (list-regex '("&allow-other-keys" "&aux" "&body" "&environment" "&key" "&optional" "&rest" "&whole")))
     :special (list *special-syntax-color* (list-regex specials))
     :function (list  *function-syntax-color* (list-regex functions))
     :boolean (list *boolean-syntax-color* (list-regex '("nil" "t")))
     :normal (list *normal-syntax-color* "."))))

(defun map-syntax (syntax text &optional syntax-map)
  (unless syntax-map
    (setf syntax-map (make-list (length text) :initial-element nil)))
  (destructuring-bind (color regex) (getf *syntax-table* syntax)
    (ppcre:do-matches (start end regex text)
      (loop :for n :from start :below end
            :unless (elt syntax-map n)
            :do (setf (elt syntax-map n)
                      (color color (elt text n)
                             :prompt-chars nil)))))
  syntax-map)

(defun highlight-text (text)
  (let ((syntax-map))
    (loop :for (syntax val) :on *syntax-table* :by #'cddr
          :do (setf syntax-map (map-syntax syntax text syntax-map)))
    (format nil "~{~a~}"
            (loop :for raw :across text
                  :for colored :in syntax-map
                  :collect (or colored raw)))))

(defmacro with-cursor-information ((line-var col-var) &body body)
  (let ((prompt (gensym "PROMPT"))
        (lines  (gensym "LINES"))
        (point  (gensym "POINT"))
        (previous-newline-position
          (gensym "PREVIOUS-NEWLINE-POSITION")))
    `(let* ((,prompt (prompt-string))
            (,lines (concatenate 'string ,prompt rl:*line-buffer*))
            (,point (+ (length ,prompt) rl:*point*))
            (,line-var (count #\newline ,lines :end ,point))
            (,col-var
              (let ((,previous-newline-position
                      (or (position #\newline ,lines
                                    :end ,point
                                    :from-end t)
                          0)))
                (- ,point ,previous-newline-position
                   (if (zerop ,line-var)
                       0
                       ;; discard the newline
                       1)))))
       (declare (ignorable ,line-var ,col-var))
       ,@body)))

(defun move-cursor-from-point-to-prompt-start ()
  (with-cursor-information (line-number col-number)
    (unless (zerop line-number)
      (format t "~c[~aF"
              ;; move cursor ~a lines up, to the beginning of line
              #\esc
              line-number))))

(defun move-cursor-from-prompt-start-to-point ()
  (with-cursor-information (line-number col-number)
    (unless (zerop line-number)
      (format t "~c[~aE"
              ;; move cursor ~a lines down, to the beginning of line
              #\esc
              line-number))
    (unless (zerop col-number)
      (format t "~c[0G~c[~aC"
              ;; ~a columns to the right
              #\esc
              #\esc
              col-number))))

(defun redisplay-with-highlight ()
  ;; This function is called when the cursor is moved, or any text is updated.
  (rl:redisplay)
  (let* ((original-point rl:*point*)
         (lines rl:*line-buffer*)
         (hl-lines (highlight-text lines)))

    (when (or *exiting-p* (zerop (length lines)))
      (return-from redisplay-with-highlight))

    (loop :initially (move-cursor-from-point-to-prompt-start)
          :for line-num :from 0
          :with nl-pos := -1
          :with old-nl-pos := nil
          :while nl-pos
          :do (setq old-nl-pos (1+ nl-pos))
              (setq nl-pos (position #\newline hl-lines
                                     :start (1+ nl-pos)))
              (let ((line (subseq hl-lines
                                  old-nl-pos
                                  nl-pos)))
                ;; Delete the existing line, then write the highlighted line
                ;; Reference: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
                ;; Below:
                ;; escape 2K: delete the entire line
                ;; escape #D: move cursor left # columns
                ;; escape #C: move cursor right # columns
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
              (when nl-pos
                (format t "~c[1E" #\esc)) ; move cursor down 1 line
          :finally (let ((nl-count (count #\newline lines)))
                     (if (zerop nl-count)
                         (format t "~c[0G~c[~aC"
                                 ;; ~a columns to the right
                                 #\esc
                                 #\esc
                                 (length (prompt-string)))
                         (format t "~c[~aF~c[~aC"
                                 ;; move cursor ~a lines up
                                 ;; and ~a columns to the right
                                 #\esc
                                 nl-count
                                 #\esc
                                 (length (prompt-string)))))
                   (move-cursor-from-prompt-start-to-point)
                   (rl:redisplay)))
  (finish-output))

(defvar *syntax-enabled* nil)

(defun enable-syntax ()
  (setf *syntax-enabled* t)
  ;; REDISPLAY function is also called when the cursor is moved.
  (rl:register-function :redisplay #'redisplay-with-highlight))

(defun disable-syntax ()
  (setf *syntax-enabled* nil)
  (rl:register-function :redisplay #'rl:redisplay))
