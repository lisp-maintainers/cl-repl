(in-package cl-repl)

(defvar *prompt-chars* t
  "Used as the default value of :PROMPT-CHARS keyword argument to COLOR")

(defun color (color string &key (prompt-chars *prompt-chars*))
  "If PROMPT-CHARS is non-NIL, then surrounds the color ascii codes in prompt
ignore characters as suggested by readline. The start character is the '\001'
which SBCL prints as #\Soh, while the end character is '\002' which SBCL
prints as #\Stx"
  (cond ((null color)
         string)
        (prompt-chars
         (format nil "~c~c[38;5;~am~c~a~c~c[0m~c"
                 #\soh
                 #\esc
                 color
                 #\stx
                 string
                 #\soh
                 #\esc
                 #\stx))
        (t
         (format nil "~c[38;5;~am~a~c[0m"
                 #\esc
                 color
                 string
                 #\esc))))

(defun strip-rl-prompt-chars (string)
  (with-output-to-string (s)
    (loop :for i :below (length string)
          :when (and (char/= #\soh (char string i))
                     (char/= #\stx (char string i)))
            :do (write-char (char string i) s))))

(defparameter *default-prompt-color* 40)
(defparameter *debugger-prompt-color* 9)
(defparameter *logo-color* 9)
(defparameter *output-indicator-color* 9)
(defparameter *splash-color* 9)
(defparameter *condition-color* 9)
(defparameter *section-color* 21)
(defparameter *message-color* 248)

(defparameter *magic-syntax-color* 39)
(defparameter *string-syntax-color* 184)
(defparameter *variable-syntax-color* 118)
(defparameter *constant-syntax-color* 118)
(defparameter *lambda-syntax-color* 39)
(defparameter *definition-syntax-color* 118)
(defparameter *keyword-syntax-color* 39)
(defparameter *special-syntax-color* 197)
(defparameter *function-syntax-color* 197)
(defparameter *boolean-syntax-color* 197)
(defparameter *normal-syntax-color*  nil)
