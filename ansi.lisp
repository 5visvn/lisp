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


;; binary search tree
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
