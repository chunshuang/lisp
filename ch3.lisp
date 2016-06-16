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
