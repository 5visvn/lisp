(car (cdr '(a b c)))


(defun w-checklist (x)
  "check if any list exists in x"
  (if (null x)
      nil
    (or (listp (car x))
        (w-checklist (cdr x)))))

(w-checklist '(1 2 3 4 (5 6)))


(defun w-count-char (c l)
  "Count char 'c' in list occurrences"
  (if (null l)
      0
    (let ((z (w-count-char c (cdr l))))
      (if (equal c (car l))
          (+ 1 z)
        z))))

(w-count-char '(a a z) '(a b c c z a))

(defun w-sum (lst)
  "sum elements in lst"
  (let ((z (remove nil lst)))
    (apply #'+ z)))
(defun w-sum-1 (lst)
  "sum elements in lst"
  (if (null lst)
      0
    (let ((x (car lst)))
      (if (null x)
          (w-sum-1 (cdr lst))
        (+ x (w-sum-1 (cdr lst)))))))

(w-sum-1 '(1 2 3))


(defun w-last-of (lst)
  "get last element of lst"
  (if (atom (cdr lst))
      (car lst)
    (w-last-of (cdr lst))))

(w-last-of '(q a s (d x)))

(car '(a b)) ; car returns element
(cdr '(a b)) ; cdr returns list

(defun w-union (lst lst1)
  "Union two lists with keeping first's order"
  (let ((res nil))
    (dolist (ele lst)
      (push ele res))
    (dolist (ele lst1)
      (pushnew ele res))
    (reverse res)))

(w-union '(a b d) '(c d))


(defun w-occurrences (lst)
  "Count each ele occurrences and ordered by times"
  lst)

(w-occurrences '(a b a d a c d c a))

(defun w-pos+ (lst)
  "Element + position"
  (let ((pos 0) (res nil))
    (dolist (ele lst)
      (progn (push (+ ele pos) res)
             (setf pos (+ 1 pos))))
    (reverse res)))
(defun w-+recursive (n lst)
  (if (null lst)
      nil
    (cons (+ n (car lst)) (w-+recursive (+ n 1) (cdr lst)))))
(defun w-+mapcar (lst)
   )

(w-pos+ '(1 2 4))
(w-+recursive 0 '(1 2 4))


;; Binary search tree
(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt
  (l nil)
  (r nil))

(defun bst-insert (obj bst <)
    (if (eql obj (node-elt bst))
        nil
      (if (funcall < obj (node-elt bst))
          (if (null (node-l bst))
              (setf (node-l bst) (make-node :elt obj))
            (bst-insert obj (node-l bst) <))
        (if (null (node-r bst))
            (setf (node-r bst) (make-node :elt obj))
            (bst-insert obj (node-r bst) <)))))

(defun bst-push (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (progn
        (bst-insert obj bst <)
        bst)))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
    (if (eql obj (node-elt bst))
        bst
      (if (funcall < obj (node-elt bst))
          (bst-find obj (node-l bst) <)
        (bst-find obj (node-r bst) <)))))

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))
    ))


(setf nums nil)

(dolist (ele '(1 3 8 5 6 2 4 9 6 4 0))
  (setf nums (bst-push ele nums #'<)))


(bst-traverse #'princ nums)

(bst-find 4 nums #'<)


(block head
  (format "Number")
  (+ 1 3)
  (if t
      (return-from head 'return-value))
  (format "Should not appear"))

;; use cond to defun bst-insert, bst-find
(defun bst-insert-cond (obj bst <)
  )

(defun bst-find-cond (obj bst <)
  (cond ((null bst)
         nil)
        ((eql obj (node-elt bst))
         bst)
        ((funcall < obj (node-elt bst))
         (bst-find-cond obj (node-l bst) <))
        (t
         (bst-find-cond obj (node-r bst) <))
        ))

(bst-find-cond 4 nums #'<)



(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))

(setf sums (maplist #'(lambda (x)
                        (apply #'+ x))
                    (reverse mon)))



(let ((x (car '(8 9 2))))
  (cons x x))
((lambda (x)
   (cons x x))
 (car '(8 9 2)))
(let* ((w (car '(3 4 1)))
       (y (+ w 7)))
  (cons w y))

((lambda (x)
  (unless (and (integerp x)
               (<= x 5))
    (* x x)))
 6)

(setf month [31 28 31 30 31 30 31 31 30 31 30 31])

(defun leap-year? (x)
  (and (zerop (mod x 4))
       (or (not (zerop (mod x 100)))
           (zerop (mod x 400)))))

(defun mon-days (m y)
  (if (and (integerp m)
           (integerp y))
      (progn
        (+ (svref month (- m 1))
           (if (and (leap-year? y) (eql m 2))
               1
             0)))
    (format "Error format")))

(let ((d 1))
  (incf d 2)
  d)


;; use function to generate a series functions
(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))
(defun combin (&rest args)
  (apply (combiner (car args))
         args))
(defun make-adder (n)
  #'(lambda (x)
    (+ x n)))

(setf add5 (make-adder 5))
(funcall add5 7)

(defun compose (&rest fns)
  (destructing-bind (fn1 . rest) (reverse fns)
                    #'(lambda (&rest args)
                        (reduce #'(lambda (v f) (funcall f v))
                                rest
                                :initial-value (apply fn1 args)))))

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conj args))))))

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun always (x) #'(lambda (&rest args) x))

;; dynamic scope
(let ((x 10))
  (defun foo ()
    (declare (special x))
    x))
(let ((x 20))
  (declare (special x))
  (foo))
(foo)

(compiled-function-p #'curry)


;; fib algorithm
;; fib (0) = fib(1) = 1
;; fib (n) = fib(n-1) + fib(n-2) ;; n>1
;; use recursive without call fib twice in a funcall
(defun fib (n)
  (if (< n 2)
      (values 1 1)
      (multiple-value-bind (x y) (fib (- n 1))
        (values (+ x y) x))))

(list (fib 10))

(multiple-value-bind (x y) (values 1 2)
  (list x y))
