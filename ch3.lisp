;; Solution to 3.2
;Write a version of union that preserves the order of the elements in
;the original lists:
;> (new-union '(a b c) '(b a d))
;(A B C D)

; note: lst-a and lst-b must be lists
(defun new-union (lst-a lst-b)
  (if (null lst-b)
      lst-a
      (if (member (car lst-b) lst-a)
          (append (new-union lst-a (cdr lst-b)) nil)
          (append (new-union lst-a (cdr lst-b)) (list (car lst-b))))))


;; Solution to 3.3
;Define a function that takes a list and returns a list indicating the
;number of times each (eql) element appears, sorted from most common
;element to least common:
;> (occurrences ' ( a b a d a c d e a ) )
;((A . 4) (C . 2) (D . 2) (B . 1))

(defun occurrences (lst)
  (sort (myoccurrences lst) #'> :key #'cdr))

(defun myoccurrences (lst)
  (if (null lst)
      nil
      (let ((elt (car lst)))
        (cons (cons elt (mycount elt 0 lst)) (myoccurrences (myremove elt (cdr lst)))))))

(defun mycount (elt n lst)
  (if (null lst)
      n
      (if (eql elt (car lst))
          (mycount elt (+ 1 n) (cdr lst))
          (mycount elt n (cdr lst)))))

(defun myremove (elt lst)
  (if (null lst)
      nil
      (if (eql elt (car lst))
          (myremove elt (cdr lst))
          (cons (car lst) (myremove elt (cdr lst))))))
