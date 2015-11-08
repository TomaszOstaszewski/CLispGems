(defun eqlistp (l1 l2)
  (cond
    ;; Both lists are null hence equal
    ((and (null l1) (null l2)) t)
    ;; One of them is null while other isn't -hence not equal
    ((or (null l1) (null l2)) nil)
    ;; cars of both lists are atoms
    ;; hence compare those atoms and then the rest of the lists
    ((and (atom (car l1)) (atom (car l2)))
     (and (eq (car l1) (car l2)) (eqlistp (cdr l1) (cdr l2))))
    ;; one of the cars is an atom while the other is not
    ;; hence not equal
    ((or (atom (car l1)) (atom (car l2))) nil)
    ;; both cars are lists
    ;; recurse on both those cars as well the cdrs of the list
    (t (and (eqlistp (cdr l1) (cdr l2))
            (eqlistp (car l1) (car l2))))))

(defun member? (lat item)
  (cond
    ((null lat) nil)
    ((eql (car lat) item) t)
    (t (member? (cdr lat) item))))

(defun set? (lat)
  (cond
    ((null lat) t)
    ((member? (cdr lat) (car lat)) nil)
    (t (set? (cdr lat)))))

(defun makeset (lat)
  (cond
    ((null lat) nil)
    ((member? (cdr lat) (car lat)) (makeset (cdr lat)))
    (t (cons (car lat) (makeset (cdr lat))))))

(defun intersect? (set1 set2)
  (cond
    ((null set1) nil)
    ((member? set2 (car set1)) t)
    (t (member? (cdr set1) set2))))

(defun subset? (set1 set2)
  (cond
    ((null set1) t)
    (t (and (member? set2 (car set1)) (subset? (cdr set1) set2)))))

(defun eqset? (set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(defun intersetc? (set1 set2)
  (cond
    ((null set1) nil)
    ((member? set2 (car set1)) t)
    (t (intersect? (cdr set1) set2))))

(defun intersect (set1 set2)
  (cond
    ((null set1) nil)
    ((member? set2 (car set1))
     (cons (car set1)
           (intersect (cdr set1) set2)))
    (t (intersect (cdr set1) set2))))

(defun set-union (set1 set2)
  (cond
    ((null set1) set2)
    ((member? set2 (car set1)) (union (cdr set1) set2))
    (t (cons (car set1) (set-union (cdr set1) set2)))))

(defun my-set-difference (set1 set2)
  (cond
    ((null set1) nil)
    ((member set2 (car set1))
     (my-set-difference (cdr set1) set2))
    (t (cons (car set1) (my-set-difference (cdr set1) set2)))))


