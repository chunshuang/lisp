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


;; Solution to 3.4
; Why does (member ' ( a ) ' ( ( a ) ( b ) ) ) return nil ?
Because 'member' uses 'eql' to compare.
> (eql '(a) '(a)
nil

Should use 'equal' instead:
> (member '(a) '((a) (b)) :test #'equal)
((a) (b))

;; Solution to 3.5
; Suppose the function pos+ takes a list and returns a list of each element
;plus its position:
;> (pos+ ' ( 7 5 1 4 ) )
;(7 6 3 7)
;Define this function using (a) recursion, (b) iteration, (c) mapcar.

(a) recursion
defun pos+ (lst)
  (pos+recur 0 lst))

(defun pos+recur (n lst)
  (if (null lst)
      nil
      (cons (cons (+ n (car lst)) nil) (pos+recur (+ 1 n) (cdr lst)))))
      
      
(b) iteration
(defun pos+ (lst)
  (do ((i 1 (+ i 1)))
      ((= i (length lst)))
    (setf (nth i lst) (+ (nth i lst) i)))
  lst)


(c) mapcar
(mapcar #'+ '(0 1 2 3) '(7 5 1 4))


;; Solution to 3.6
(a) cons: a pair of pointer, the first one is cdr and the second one is car.
(b) list: a cons whose first pointer points to the first element of a list, and the second pointer points the rest of the
          list
(c) length: 
     (defun length (lst)
      (if (null lst)
          0
          (+ 1 (length (car lst))))) ; car points to the rest of the list
(d) member:
      (defun member (elt lst)
        (if (null lst)
            nil
            (if (eql elt (cdr lst))
                lst
                (member elt (car lst))))) ; car/cdr reverse


;; Solution to 3.7
; Modify the program in Figure 3.6 to use fewer cons cells. (Hint: use dotted list)
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt) ; the only change is using 'cons' instead of 'list'
      elt))
