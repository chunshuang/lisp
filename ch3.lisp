(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))



(defun new-union (lst-a lst-b) ; no termination condition
  (if (null lst-b)
      lst-a
      (if (member (car lst-b) lst-a)
          (append (new-union lst-a (cdr lst-b)) nil)
          (append (new-union lst-a (cdr lst-b)) (list (car lst-b))))))
