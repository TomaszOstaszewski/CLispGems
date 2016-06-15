(defstruct s-diet-node bottom top left right)

;; Go to the right subtree
;; Returns the following values:
;; - bottom boundary of a rightmost leaf
;; - top boundary of a rightmost leaf
;; - a subtree without a rightmost most leaf
(defun split-max (a-diet-node)
  (if (null a-diet-node)
      (values nil nil nil)
      (with-slots (bottom top left right) a-diet-node
        (if (null right)
            (values bottom top left)
            (multiple-value-bind (max-bottom max-top subtree-w/o-max) (split-max right)
              (values max-bottom max-top
                      (make-s-diet-node :bottom bottom :top top :left left :right subtree-w/o-max)))))))

;; Go to the left subtree
;; Returns the following values:
;; - bottom boundary of a leftmost leaf
;; - top boundary of a leftmost leaf
;; - a subtree without a leftmost leaf
(defun split-min (a-diet-node)
  (cond
    ((null a-diet-node) (values nil nil nil))
    ((null (s-diet-node-left a-diet-node))
     (values
      (s-diet-node-bottom a-diet-node)
      (s-diet-node-top a-diet-node)
      (s-diet-node-right a-diet-node)))
    (t (multiple-value-bind (bottom top tree-w/o-min-interval)
           (split-min (s-diet-node-left a-diet-node))
         (values bottom top (make-s-diet-node
                             :bottom (s-diet-node-bottom a-diet-node)
                             :top (s-diet-node-top a-diet-node)
                             :right (s-diet-node-right a-diet-node)
                             :left tree-w/o-min-interval))))))

(defun join-left (a-diet-node)
  (with-slots (top bottom left right) a-diet-node
    (if (null left)
        a-diet-node
        (multiple-value-bind (max-int-bottom max-int-top l-subtree-w/o-max-interval)
            (split-max left)
          (if (eql (1+ max-int-top) bottom)
              (make-s-diet-node :bottom max-int-bottom :top top :left l-subtree-w/o-max-interval :right right)
              (make-s-diet-node :bottom bottom :top top :left left :right right))))))

(defun join-right (a-diet-node)
  (with-slots (top bottom left right) a-diet-node
    (if (null right)
        a-diet-node
        (multiple-value-bind (min-int-bottom min-int-top r-subtree-w/o-min-interval)
            (split-min right)
          (if (eql (1+ top) min-int-bottom)
              (make-s-diet-node :bottom bottom :top min-int-top :left left :right r-subtree-w/o-min-interval)
              (make-s-diet-node :bottom bottom :top top :left left :right right))))))

(defun diet-insert (value a-diet-node)
  (if (null a-diet-node)
      (make-s-diet-node :bottom value :top value :left nil :right nil)
      (with-slots (bottom top left right) a-diet-node
        (cond
          ((< value bottom)
           (if (eql (1+ value) bottom)
               (join-left (make-s-diet-node :bottom value :top top :left left :right right))
               (make-s-diet-node :bottom bottom :top top :left (diet-insert value left) :right right)))
          ((> value top)
           (if (eql value (1+ top))
               (join-right (make-s-diet-node :bottom bottom :top value :left left :right right))
               (make-s-diet-node :bottom bottom :top top :left left :right (diet-insert value right))))
          (t a-diet-node)))))

(defun diet-merge (a-tree)
  (with-slots (left right) a-tree
    (cond
      ((null right) left)
      ((null left) right)
      (t
       (if (zerop (random 2))
           (multiple-value-bind (bottom top new-left-subtree) (split-max left)
             (make-s-diet-node :bottom bottom :top top :left new-left-subtree :right right))
           (multiple-value-bind (bottom top new-right-subtree) (split-min right)
             (make-s-diet-node :bottom bottom :top top :left left :right new-right-subtree)))))))

(defun diet-remove (a-val a-tree)
  (cond
    ;; Empty tree - trivial
    ((null a-tree) nil)
    (t
     (with-slots (bottom top left right) a-tree
       (assert (and (not (null bottom)) (not (null top))))
       (cond
         ;; Lower than a bottom boundary
         ;; advance to a left subtree
         ((< a-val bottom)
          (make-s-diet-node :bottom bottom :top top :left (diet-remove a-val left) :right right))
         ;; Higher than a top boundary
         ;; advance to a right subtree
         ((> a-val top)
          (make-s-diet-node :bottom bottom :top top :left left :right (diet-remove a-val right)))
         ;; a-val belongs to the interval [bottom; top].
         ;; Is it a single value interval?
         ((eql bottom top) (diet-merge a-tree))
         ;; Do we remove a bottom part?
         ((eql a-val bottom) (make-s-diet-node :bottom (1+ bottom) :top top :left left :right right))
         ;; Do we remova a top part?
         ((eql a-val top) (make-s-diet-node :bottom bottom :top (1- top) :left left :right right))
         ;; Neither a single value interval
         ;; nor we shrank either bottom or top
         ;; Need to split into 2 new nodes.
         ;; We chose the split pattern in a random fashion
         ;;
         (t
          (if (zerop (random 2))
              (make-s-diet-node :bottom bottom :top (1- a-val) :left left
                                :right (make-s-diet-node :bottom (1+ a-val) :top top :left nil :right right))
              (make-s-diet-node :bottom (1+ a-val) :top top :right right
                                :left (make-s-diet-node :bottom bottom :top (1- a-val) :left left :right nil)))))))))

(defun diet-height (a-tree)
  (if (null a-tree)
      0
      (with-slots (left right) a-tree
        (cond
          ((and (null right) (null left)) 0)
          ((null left) (1+ (diet-height right)))
          ((null right) (1+ (diet-height left)))
          (t
           (1+ (max (diet-height right) (diet-height left))))))))

(defun diet-balance (a-tree)
  (cond
    ((null a-tree) 0)
    ((and (null (s-diet-node-left a-tree)) (null (s-diet-node-right a-tree))) 0)
    (t (- (diet-height (s-diet-node-right a-tree))
          (diet-height (s-diet-node-left a-tree))))))

(defun diet-longest-interval (a-tree)
  (if (null a-tree)
      (values 0 nil)
      (with-slots (bottom top left right) a-tree
        (let ((curr-interval (1+ (- top bottom))))
          (cond
            ((and (null left) (null right)) (values curr-interval a-tree))
            ((null left) (multiple-value-bind (max-subtree-interval node) (diet-max-interval right)
                           (if (> max-subtree-interval curr-interval)
                               (values max-subtree-interval node)
                               (values curr-interval a-tree))))
            ((null right) (multiple-value-bind (max-subtree-interval node) (diet-max-interval left)
                             (if (> max-subtree-interval curr-interval)
                                 (values max-subtree-interval node)
                                 (values curr-interval a-tree))))
            (t
             (multiple-value-bind (max-left-int max-left-node)
                 (diet-max-interval left)
               (multiple-value-bind (max-right-int max-right-node)
                   (diet-max-interval right)
                 (if (> max-left-int max-right-int)
                     (values max-left-int max-left-node)
                     (values max-right-int max-right-node))))))))))

(defun diet-r-rotate (a-tree)
  (if (null a-tree)
      nil
      (with-slots ((l left) (r right)) a-tree
        (if (or (null l) (null r))
            a-tree
            (with-slots ((ll left) (lr right)) l
              (make-s-diet-node :bottom (s-diet-node-bottom l) :top (s-diet-node-top l)
                                :left ll
                                :right (make-s-diet-node :bottom (s-diet-node-bottom a-tree) :top (s-diet-node-top a-tree)
                                                         :left lr :right r)))))))

(defun diet-l-rotate (a-tree)
  (if (null a-tree)
      nil
      (with-slots ((l left) (r right))  a-tree
        (if (or (null l) (null r))
            a-tree
            (with-slots ((rl left) (rr right)) r
              (make-s-diet-node :bottom (s-diet-node-bottom r) :top (s-diet-node-top r)
                                :left (make-s-diet-node :bottom (s-diet-node-bottom a-tree) :top (s-diet-node-top a-tree)
                                                        :left l :right rl)
                                :right rr))))))

(defun diet-count-intervals (a-tree)
  (if (null a-tree)
      0
      (with-slots (left right) a-tree
        (if (and (null left) (null right))
            1
            (+ 1 (diet-count-intervals left) (diet-count-intervals right))))))
