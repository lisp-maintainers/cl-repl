(in-package :cl-repl)

;; Wrapper over cl-readline to support unicode. More or less. It keeps translating from
;; the readline "bytes" representation, where λ3 is 3 bytes, to the sbcl et al representation,
;; where (length "λx") is 2, not 3.
;;
;; The rest of the program should always go through this helpers, and never use rl: directly, to
;; avoid bugs when typing non-ascii characters.

(defun char-size (chr)
  "Size of chr in bytes"
  (ceiling (/ (log (char-code chr) 2) 8)))

(defun byte-position->sbcl-position (str point)
  "
If I'm on ofoλ|bar, I'm on the 4th char, but rl:*point* would be 5 (the λ char takes 2 bytes).
Given 'ofoλbar', and point = 5, this function would return 4.
"
  (let ((total-bytes-consumed 0)
        (sbcl-position 0))
    (loop for c across str
          until (>= total-bytes-consumed point)
          do (incf total-bytes-consumed (char-size c))
          when (<= total-bytes-consumed point) do (incf sbcl-position)
          finally (return sbcl-position))))

(defun sbcl-position->byte-position (str point)
  "Reverse of byte-position->sbcl-position"
  (let ((substr (subseq str 0 point)))
    (loop for c across substr sum (char-size c))))

(defun set-readline-point (val)
  "In ASCII, equivalent to (setf rl:*point* val)"
  (setf rl:*point* (sbcl-position->byte-position rl:*line-buffer* val)))

(defun readline-point ()
  "In ASCII, equivalent to rl:*point*)"
  (let ((cool-point (byte-position->sbcl-position rl:*line-buffer* rl:*point*)))
    cool-point))

(defun readline-line-buffer ()
  "In ASCII, equivalent to rl:*line-buffer*"
  rl:*line-buffer*)
