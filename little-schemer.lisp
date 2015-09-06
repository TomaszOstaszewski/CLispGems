(defun eqlistp (l1 l2)
           (cond 
             ((and (null l1) (null l2)) t)
             ((or (null l1) (null l2)) nil)
             ((and (atom (car l1)) (atom (car l2)))
              (and (eqlistp (cdr l1) (cdr l2))
                   (eq (car l1) (car l2))))
             ((or (atom (car l1)) (atom (car l2))) nil)
             (t (and (eqlistp (cdr l1) (cdr l2))
                     (eqlistp (car l1) (car l2))))))

(defun member? (lat item)
  (cond
    ((null lat) nil)
    ((eql (car lat) item) t)
    (t (member? (cdr lat) item))))
     
