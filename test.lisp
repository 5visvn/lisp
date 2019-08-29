(defun helloworld ()
  (format t "Hello world!"))

;; record cd
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

;; add records for test
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

;; show db by format
(defun dump-db ()
   (dolist (cd *db*)
     (format t "~{~a:~10t~a~%~}~%" cd)))
;; ~a consume a parameter
;; ~% enter, another line
;; ~10t means text align in 10 space, like \t in c++

;; read record from user input
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; save db
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; load db
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; get ord number of a list
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not (lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))

;; select db
(defun select-artist (artist)
  (remove-if-not
   (lambda (cd)
     (equal (getf cd :artist) artist))
  *db*))
;; common selector
;; (where-one :title "Home")
(defun where-one (key value)
  (remove-if-not
   (lambda (cd)
     (equal value
            (getf cd key)))
   *db*))

;; SQL where
;; where multiple conditions
;; (where :artist "Dixie Chicks" :rating 8)
(defun where (&key title artist rating (ripped nil ripped-p))
                ;; (remove-if-not
                 (lambda (cd)
                   (and
                    (if title  (equal title (getf cd :title)) t)
                    (if artist  (equal artist (getf cd :artist)) t)
                    (if rating  (equal rating (getf cd :rating)) t)
                    (if ripped-p  (equal ripped (getf cd :ripped)) t)
                    ))
                 ;; *db*)
                )

(defun select (where-fn)
  (remove-if-not where-fn *db*)
  )

;; ;; SQL select
;; ;; only selected key-word will display
;; (defun select-one (&key filters cd)
;;   ;; remove values not in filter
;;   )

;; ;; (select :title (where :artist "Dixie Chicks" :rating 8))
;; (defun select (&key filters cds)
;;   (if filters t (setq filters (:title :artist :rating :ripped))
;;   (if cds
;;       (dolist (cd cds)
;;         (select-one cd))
;;       (dolist (cd *db*)
;;         (select-one cd))
;;       )
;;

(defun update (where-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
              (when (funcall where-fn row)
                (if title  (setf (getf row :title) title))
                (if artist  (setf (getf row :artist) artist))
                (if rating  (setf (getf row :rating) rating))
                (if ripped-p  (setf (getf row :ripped) ripped)))
              row) *db*)))

(defun delete-cd (where-fn)
  (setf *db* (remove-if where-fn *db*)))

(defmacro backwards (expr) (reverse expr))

;; use macro to generate where function
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comaprison-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where-mac (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comaprison-list clauses))))
;; `(1 2 ,@(list 3 4 5) 6 7) -> (1 2 3 4 5 6 7)



;;;;  chapter 5 function

;;; parameters of funtion
;;; optional
(defun foo51 (a &optional (b 10))
  (list a b))
;; (foo 1)      -> (1 10)
;; (foo 1 2)    -> (1 2)
(defun make-rectangle (width &optional (height width))
  (list width height))
;; (make-rectangle 1)   -> (1 1)
;; (make-rectangle 1 2) -> (1 2)
;; (make-rectangle 2)   -> (2 2)
(defun foo52 (a b &optional (c 3 c-supplied-p)) ; c-supplied-p could be any name, by recomend this one.
  (list a b c c-supplied-p))
;; (foo5.2 1 2)  ->  (1 2 3 NIL)
;; (foo5.2 1 2 4)  ->  (1 2 4 T)

;;; rest
(defun x (a &rest numbers)
  `(,a ,@numbers))

;;; key
(defun foo53 (&key ((:app a)) ((:box b) 1) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))
;; (foo53 :app 1 :box 20 :charlie 3)   ->   (1 20 3 T)

;;; different parameter's types can be appeared in the same function(is seems not support all in one during test),
;;; but the order should be: required > &optional > &rest > &key
;;; compiler WARNING &optional + &key : because optional will consume keywords of key if this optional parameter was not provided
;;; &rest + &key should notice that &rest will include all &key's keywords and values, and &key is correct set, and when input parameters of &rest, should input &key's first, do not start with any other &rest value
;;; &optional + &rest + &key seems not supported
;;;
(defun foo54 (a &optional (b 0 b-supplied-p) &rest c)
  `(,a ,b ,c ,b-supplied-p))
(defun foo55 (a &rest b &key c d e)
  (list a b c d e))
;; (foo55 1 :c 6 :d 7 :e 8)  -> (1 (:C 6 :D 7 :E 8) 6 7 8)

;;;; funtion return value
(defun foo56 (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo56 (list i j)))))) ; you need to declare function name
;; (funcall (function foo56) 12)  ->  (2 7)


;;;; function as an object
;;; function=#'
;;; funcall  -> parameters pass by list
(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do (format t "*"))
       (format t "~%")))
;; (plot #'exp 0 4 1/2)

;;; apply  -> parameters passed one by one
(defvar plot-data (list 0 4 1/2))
;; (apply #'plot #'exp plot-data)

;;;; lambda
(apply #'(lambda (x y) (* x y)) (list 2 3))
;; (funcall #'(lambda (x y) (* x y))  2 3)





;;;; variables
(defvar test-times t)

;;; let : create a dynamic variable(local variable)
;;; set : change the value of variable(parse parameter by reference)
(defun foo61 (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter: ~a~%" x))

(defun foo62 (x)
  (format t "Parameter: ~a~%" x)
  (let* ((x 10) (y (+ x 10)))
    (format t "Parameter x: ~a~%" x)
    (format t "Parameter y: ~a~%" y))
  (format t "Parameter: ~a~%" x))

(defparameter *fn*
  (let ((count 0))
    #'(lambda ()
        (setf count (1+ count)))))
    ;; #'(lambda () (incf count))
    ;; #'(lambda () (decf count))
    ;; #'(lambda () count)))

;;; The difference between defvar & defparameter is : defparameter always set initial value to variable. defvar only set initial value when variable has not value, and defvar allows no initial value.
;;; defvar -> static value
;;; defparamter -> temporary value

;;; constant
;; (defconstant name+ "constant value" "documentation-string")
;;; defconstant's variable is global, not allowed to be passed to a function

;;; setf
;; variable
; (setf x 10)
;; array: a[0] = 10 in c++
; (setf (aref a 0) 10)
;; Hash table: hash['key'] = 10
; (setf (gethash 'key hash) 10)
;; slot named 'field': o.field = 10
; (setf (field o) 10)

;;; rotate : swap a b
; (rotatef a b)
;;; shiftf : move a position of values to left
; (shiftf a b 10) ; a = b, b = 10






;;;; macro
;;; 7 common macros

;;; condition
;; if : single body
(defun foo74 ()
  (if (> 2 3)
      (print "yes")
      (print "nop"))) ; -> "nop"
;; if progn
(defun foo75 ()
  (if (< 2 3)
      (progn
        (print "2 < 3 ?")
        (print "yes"))
      (print "nop"))) ; -> "2 < 3 ?" "yes"
;; when & unless
(defun foo76 ()
  (when (< 2 3)
    (print "yes")))
(defun foo77 ()
  (unless (> 2 3)
    (print "yes")))
;; cond
(defun foo78 ()
  (cond ((> 2 3) (print "2 > 3"))
        ((> 2 4) (print "2 > 4"))
        ((> 2 1) (print "2 > 1") (print "another do") nil )))
;; and, or, not
; (not nil)
; (or t nil)
; (and t nil)

;;; loop
;; dolist
(defun foo72 ()
  (dolist (x `(1 2 3))
    (print x)
    (if (evenp x) (return))))
;; dotimes
(defun foo73 ()
  (dotimes (x 10)
    (print x)))
;; do
(defun foo71 ()
  (do ((i 0 (+ 1 i)))
      ((>= i 10))
    (format t "~%~d" i)))
;; loop : break when (return)
(defun foo79 ()
  (defparameter break-time (+ (get-universal-time) 10))
  (format t "scheduled time: ~a~%" break-time)
  (format t "current time: ~a" (get-universal-time))
  (loop
     (when (> (get-universal-time) break-time) (return))
     (print "waiting")
     (sleep 3))
  (print "break")
  t)
(defun foo710()
  (format t "generate list using do: ~a~%"
          (do ((num nil) (i 1 (+ i 1)))
              ((< i 10) (nreverse num))
            (push i num)))
  (format t "generate list using loop: ~a~%"
          (loop for i from 1 to 10 collecting i))
  (format t "sum squrt from 1 to 10: ~a~%"
          (loop for x from 1 to 10 suming (exp x 2)))
  (format t "find aeiou: ~a~%"
          (loop for x across "the quick brown forx jumps over the lazy dog"
             counting (find x "aeiou")))
  (format t "get fibonacci 10th: "
          (loop for x blow 10
             and a = 0 then b
             and  b = 1 then (+ b a)
               finally (return a))))



;;;; custom macros

;; do-primes macro : find a primes list in a given range
(defun is-prime (num)
  (when (> number 1)
    (loop for i from 2 to (isqrt num)
         (if (!= 0 (mod num i))
             (return nil)))
    t)
  nil)

(defun doprime (var-range)
  (loop for x from (first-of var-range) to (second-of var-range)
       (if (is-prime x) collecting x)))



(defmarco do-primes (var-and-rang &rest body)

  )

