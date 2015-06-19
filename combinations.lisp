
#| 
A simple combinations generator


|#

                                        ;(defpackage :
                                        ;(in-package :org.ostaszewski.tomasz.permutations)

                                        ;(declaim (optimize (debug 3) (speed 0) (safety 3)))

(declaim (optimize (debug 0) (speed 3) (safety 0)))

(defstruct comb
  vec
  is-leaf
  n
  k)

(defstruct k-comb
  vec
  last_j)

(declaim (inline find-next-item))

(defun find-next-item (vec level min max)
  (declare (fixnum level min max))
  (declare (type (simple-array signed-byte) vec))
  (loop for idx of-type fixnum from min to max
     when (not (find idx vec :end level)) return idx))

(defun rec-f (vec level min max)
  (declare (fixnum level min max))
  (declare (type (simple-array signed-byte) vec))
  (let ((next-item (find-next-item vec level min max)))
    (cond
      (next-item
       (setf (svref vec level) next-item)
       (if (eql (1+ level) (length vec))
           (return-from rec-f vec)
           (rec-f vec (1+ level) (1+ (the fixnum (svref vec level))) max)))
      (t
       (unless (eql level 0)
         (decf level)
         (rec-f vec level (1+ (the fixnum (svref vec level))) max))))))

;;;
(defun create (n k)
  (declare (fixnum n k))
  (make-comb
   :vec (make-array k :element-type 'signed-byte)
   :is-leaf nil
   :n (1- n)))

(defmacro while (a-cond &body body)
  (let ((g1 (gensym)))
    `(tagbody
        ,g1
        (when ,a-cond
          ,@body
          (go , g1)))))

(defmacro vec-last (v)
  (let ((g (gensym)))
    `(let ((,g ,v))
       (svref ,g (1- (length ,g))))))

(defun find-next-comb (a-comb)
  (with-slots (vec is-leaf n) a-comb
    (declare (fixnum n))
    (declare (type (simple-array signed-byte) vec))
    (let ((next-comb
           (if is-leaf
               (rec-f vec (1- (length vec))
                      (1+ (the fixnum (vec-last vec))) n)
               (rec-f vec 0 0 n))))
      (if next-comb
          (setf is-leaf t)
          (setf is-leaf nil))
      next-comb)))

(defun comb-knuth-ctor (n a-comb)
  (with-slots (vec last_j) a-comb
    (declare (type (simple-array signed-byte) vec))
    (let ((k (- (length vec) 2)))
      (setf (svref vec k) n
            (svref vec (+ 1 k)) 0
            last_j -1)
      (do ((idx 0 (1+ idx)))
          ((eql idx k) vec)
        (setf (svref vec idx) idx)))))


(defun comb-knuth-create (n k)
  (declare (fixnum n k))
  (let ((retval (make-k-comb
                 :vec (make-array (+ 2 k) :element-type 'unsigned-byte))))
    (comb-knuth-ctor n retval)
    retval))

(defun l-next-comb (a-k-comb)
  (with-slots (vec) a-k-comb
    (declare (type (simple-array unsigned-byte) vec))
    (subseq vec 0 (- (length vec) 2))))


(defun l-find-next-comb (a-k-comb)
  (with-slots (vec last_j) a-k-comb
    (declare (type (simple-array unsigned-byte) vec))
    (declare (fixnum last_j))
    (let ((k (- (length vec) 2)))
      ;; Search for an index
      ;; that points at the bump in
      ;; the sequence
      (when (< last_j 0)
        (let ((l-j 0))
          (declare (fixnum l-j))
          (while (and (< l-j k)
                      (eql (the fixnum (svref vec (1+ l-j))) (the fixnum (1+ (the fixnum (svref vec l-j))))))
            (setf (svref vec l-j) l-j)
            (incf l-j))
          (setf last_j l-j)))
      (when (eql last_j k)
	(comb-knuth-ctor (svref vec k) a-k-comb)
	(return-from l-find-next-comb nil))
      (incf (the fixnum (svref vec last_j)))
      (decf last_j)
      t)))

#|
(defun l-find-next-comb (a-k-comb)
  (with-slots (vec last_j) a-k-comb
    (declare (type (simple-array unsigned-byte) vec))
    (declare (fixnum last_j))
    (let ((k (- (length vec) 2)))
      ;; Search for an index
      ;; that points at the bump in
      ;; the sequence
      (let ((l-j 0))
	(declare (fixnum l-j))
	(while (and (< l-j k)
		    (eql (the fixnum (svref vec (1+ l-j))) (the fixnum (1+ (the fixnum (svref vec l-j))))))
	  (setf (svref vec l-j) l-j)
	  (incf l-j))
        (when (eql l-j k)
          (comb-knuth-ctor (svref vec k) a-k-comb)
          (return-from l-find-next-comb nil))
        (incf (the fixnum (svref vec l-j)))
        t))))
|#

(defun test-comb (n k)
  (time (let ((x (comb-knuth-create n k)) (count 0))
	  (declare (fixnum count))
	  (do ((c (l-find-next-comb x) (l-find-next-comb x)))
	      ((null c) (prog1 count (pprint x)))
            (pprint x)
	    (incf count)))))

(defun append-front-0 (l)
  (mapcar #'identity l))

(defun append-front-1 (l n)
  (declare (fixnum n))
  (mapcar #'(lambda(x) (the fixnum (+ (the fixnum (expt 2 n)) (the fixnum x)))) l))

(defun gray-code (n)
  (declare (fixnum n))
  (case n
    (0 nil)
    (1 (list 0 1))
    (t (let ((p (gray-code (1- n))))
         (append p (reverse (append-front-1 p (1- n))))))))

(declaim (inline
	  knuth-rd-even-odd-helper
	  knuth-rd-even-odd-helper-2
	  knuth-rd-even
	  knuth-rd-odd
	  ))

(defun knuth-rd-even-odd-helper (a-k-comb j)
  (when (< (1+ (the fixnum (svref a-k-comb j)))
	   (the fixnum (svref a-k-comb (1+ j))))
    (setf
     (svref a-k-comb (1- j)) (the fixnum (svref a-k-comb j))
     (the fixnum (svref a-k-comb j)) (1+ (the fixnum (svref a-k-comb j))))))

(defun knuth-rd-even-odd-helper-2 (a-k-comb j)
  (when (>= (the fixnum (svref a-k-comb j)) j)
    (setf
     (svref a-k-comb j) (svref a-k-comb (1- j))
     (svref a-k-comb (1- j)) (- j 2))))

(defun knuth-R3-odd (a-k-comb)
  (declare (type (simple-array signed-byte) a-k-comb))
  (let ((tmp (the fixnum (1+ (the fixnum (svref a-k-comb 1))))))
    ;; Knuth's step R3 for odd case
    (if (< tmp (the fixnum (svref a-k-comb 2)))
        (progn (setf (svref a-k-comb 1) tmp)
               (return-from knuth-R3-odd t))
        (let ((j 2)
              (k (- (length a-k-comb) 2)))
          (declare (fixnum  j k))
          ;; Knuth's step R4
          (while (<= j k)
            (when (knuth-rd-even-odd-helper-2 a-k-comb j)
              (return-from knuth-R3-odd t))
            (incf j)
            (when (knuth-rd-even-odd-helper a-k-comb j) 
              (return-from knuth-R3-odd t))
            (incf j))))))

(defun knuth-R3-even (a-k-comb)
  (declare (type (simple-array signed-byte) a-k-comb))
  (when (> (the fixnum (svref a-k-comb 1)) 0)
    (decf (the fixnum (svref a-k-comb 1)))
    (return-from knuth-R3-even t))
  (let ((j 2)
	(k  (- (length a-k-comb) 2)))
    (declare (fixnum j k))
    (when (knuth-rd-even-odd-helper a-k-comb j)
      (return-from knuth-R3-even t))
    (incf j)
    (while (<= j k)
      (when (knuth-rd-even-odd-helper-2 a-k-comb j)
	(return-from knuth-R3-even t))
      (incf j)
      (when (knuth-rd-even-odd-helper a-k-comb j)
	(return-from knuth-R3-even t))
      (incf j))))

(defun knuth-rd-comb (a-k-comb)
  (declare (type (simple-array signed-byte) a-k-comb))
  (let* ((k (- (length a-k-comb) 2)))
    (case (mod k 2)
      (1 (knuth-R3-odd a-k-comb))
      (0 (knuth-R3-even a-k-comb)))))

(defun knuth-rd-comb-create (n k)
  (declare (fixnum n k))
  (let ((retval (make-array (+ k 2) :element-type 'fixnum)))
    (setf (aref retval (1+ k)) n)
    (dotimes (idx k)
      (setf (aref retval (1+ idx)) idx))
    retval))

(defun knuth-rd-0-based-odd (comb-vec)
  (let ((c0 (svref comb-vec 0))
	(c1 (svref comb-vec 1))
	(k (1- (length comb-vec))))
    (when (< (1+ c0) c1)
      (incf (svref comb-vec 0))
      (return-from knuth-rd-0-based-odd t))
    (let ((j 1))
      (while (< j k)
	(when (> (svref comb-vec j) j)
	  (return-from knuth-rd-0-based-odd 
	    (setf (svref comb-vec j) (svref comb-vec (1- j))
		  (svref comb-vec (1- j)) (- j 1))))
	(incf j)
	(when (< (1+ (svref comb-vec j)) (svref comb-vec (1+ j)))
	  (return-from knuth-rd-0-based-odd
	    (setf (svref comb-vec (1- j)) (svref comb-vec j)
		  (svref comb-vec j) (1+ (svref comb-vec j)))))
	(incf j)))))

(defun knuth-rd-0-based-even (comb-vec)
  (declare (type (simple-array fixnum)))
  (when (> (the fixnum (svref comb-vec 0)) 0)
    (decf (the fixnum (svref comb-vec 0)))
    (return-from knuth-rd-0-based-even t))
  (let ((j 1)
	(k (1- (length comb-vec))))
    (when (< (1+ (svref comb-vec j)) (svref comb-vec (1+ j)))
      (return-from knuth-rd-0-based-even
	(setf (svref comb-vec (1- j)) (svref comb-vec j)
	      (svref comb-vec j) (1+ (svref comb-vec j)))))
    (incf j)
    (while (< j k)
      (when (> (svref comb-vec j) j)
	(return-from knuth-rd-0-based-even
	  (setf (svref comb-vec j) (svref comb-vec (1- j))
		(svref comb-vec (1- j)) (- j 1))))
      (incf j)
      (when (< (1+ (svref comb-vec j)) (svref comb-vec (1+ j)))
	(return-from knuth-rd-0-based-even
	  (setf (svref comb-vec (1- j)) (svref comb-vec j)
		(svref comb-vec j) (1+ (svref comb-vec j)))))
      (incf j))))


