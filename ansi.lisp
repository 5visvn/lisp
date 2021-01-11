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
