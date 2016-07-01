(defstruct rb-node val (color :black) (left nil) (right nil))

(defun rb-search (v t-r col)
  (labels ((t-rec (a-t path col)
             (if (null a-t)
                 (funcall col 'f path)
                 (let ((new-path (cons a-t path)))
                   (with-slots (val (l left) (r right)) a-t
                     (cond 
                       ((< v val) (t-rec l new-path #'(lambda(f p) (funcall col f p))))
                       ((> v val) (t-rec r new-path #'(lambda(f p) (funcall col f p))))
                       (t (funcall col 't new-path))))))))
    (t-rec t-r nil col)))

(defun rb-search-2 (v t-r)
  (labels ((t-rec (a-t p gp)
             (if (null a-t)
                 (list nil p gp)
                 (with-slots (val (l left) (r right)) a-t
                   (cond 
                     ((< v val) (t-rec l a-t p))
                     ((> v val) (t-rec r a-t gp))
                     (t (list a-t p gp)))))))
    (t-rec t-r nil nil)))

(defun rb-search-3 (v t-r col)
  (labels ((t-rec (a-t p gp)
             (if (null a-t)
                 (funcall col nil p gp)
                 (with-slots (val (l left) (r right)) a-t
                   (cond 
                     ((< v val) (t-rec l a-t p))
                     ((> v val) (t-rec r a-t p))
                     (t (funcall col a-t p gp)))))))
    (t-rec t-r nil nil)))
