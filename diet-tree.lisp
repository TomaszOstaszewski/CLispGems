(defstruct s-diet-node bottom top left right)

(defun all-but-rightmost-leaf (a-tree)
  (cond
    ((null a-tree) nil)
    ((null (third a-tree)) (values (first a-tree) nil))
    (t (multiple-value-bind (node rest)
           (all-but-rightmost-leaf (third a-tree))
         (values node (list (first a-tree) (second a-tree) rest))))))

;; Go to the right subtree
;; Returns the following values:
;; - bottom boundary of a rightmost leaf
;; - top boundary of a rightmost leaf
;; - a subtree without a rightmost most leaf
(defun split-max (a-diet-node)
  (cond
    ((null a-diet-node) (values nil nil nil))
    ((null (s-diet-node-right a-diet-node))
     (values
      (s-diet-node-bottom a-diet-node)
      (s-diet-node-top a-diet-node)
      (s-diet-node-left a-diet-node)))
    (t (multiple-value-bind (bottom top rest)
           (split-max (s-diet-node-right a-diet-node))
         (values bottom top (make-s-diet-node
                             :bottom (s-diet-node-bottom a-diet-node)
                             :top (s-diet-node-top a-diet-node)
                             :right rest
                             :left (s-diet-node-left a-diet-node)))))))
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
  (let ((left-subtree (s-diet-node-left a-diet-node)))
    (if (null left-subtree)
        a-diet-node
        (multiple-value-bind (bottom top tree-w/o-max-interval)
            (split-max left-subtree)
          (if (eql (1+ top) (s-diet-node-bottom a-diet-node))
              (make-s-diet-node :bottom bottom
                                :top (s-diet-node-top a-diet-node)
                                :left tree-w/o-max-interval
                                :right (s-diet-node-right a-diet-node))
              (make-s-diet-node :bottom (s-diet-node-bottom a-diet-node)
                                :top (s-diet-node-top a-diet-node)
                                :left left-subtree
                                :right (s-diet-node-right a-diet-node)))))))

(defun join-right (a-diet-node)
  (let ((right-subtree (s-diet-node-right a-diet-node)))
    (if (null right-subtree)
        a-diet-node
        (multiple-value-bind (bottom top tree-w/o-max-interval)
            (split-min right-subtree)
          (if (eql (1+ (s-diet-node-top a-diet-node)) bottom)
              (make-s-diet-node :bottom (s-diet-node-bottom a-diet-node)
                                :top bottom
                                :left (s-diet-node-left a-diet-node)
                                :right tree-w/o-max-interval)
              (make-s-diet-node :bottom (s-diet-node-bottom a-diet-node)
                                :top (s-diet-node-top a-diet-node)
                                :left (s-diet-node-left a-diet-node)
                                :right (s-diet-node-right a-diet-node)))))))

(defun diet-insert (value a-diet-node)
  (cond
    ((null a-diet-node) (make-s-diet-node :bottom value
                                          :top value
                                          :left nil
                                          :right nil))
    ((< value (s-diet-node-bottom a-diet-node))
     (if (eql (1+ value) (s-diet-node-bottom a-diet-node))
         (join-left (make-s-diet-node
                     :bottom value
                     :top (s-diet-node-top a-diet-node)
                     :left (s-diet-node-left a-diet-node)
                     :right (s-diet-node-right a-diet-node)))
         (make-s-diet-node
          :bottom (s-diet-node-bottom a-diet-node)
          :top (s-diet-node-top a-diet-node)
          :left (diet-insert value (s-diet-node-left a-diet-node))
          :right (s-diet-node-right a-diet-node))))
    ((> value (s-diet-node-top a-diet-node))
     (if (eql value (1+ (s-diet-node-top a-diet-node)))
         (join-right (make-s-diet-node
                      :bottom (s-diet-node-bottom a-diet-node)
                      :top value
                      :left (s-diet-node-left a-diet-node)
                      :right (s-diet-node-right a-diet-node)))
         (make-s-diet-node
          :bottom (s-diet-node-bottom a-diet-node)
          :top (s-diet-node-top a-diet-node)
          :left (s-diet-node-left a-diet-node)
          :right (diet-insert value (s-diet-node-right a-diet-node)))))
    (t a-diet-node)))

(defun diet-merge (a-tree)
  (cond
    ((null (s-diet-node-right a-tree)) (s-diet-node-left a-tree))
    ((null (s-diet-node-left a-tree)) (s-diet-node-right a-tree))
    (t
     (if (zerop (random 2))
         (multiple-value-bind (bottom top new-left-subtree)
             (split-max (s-diet-node-left a-tree))
           (make-s-diet-node :bottom bottom :top top
                             :left new-left-subtree :right (s-diet-node-right a-tree)))
         (multiple-value-bind (bottom top new-right-subtree)
             (split-min (s-diet-node-right a-tree))
           (make-s-diet-node :bottom bottom :top top
                             :left (s-diet-node-left a-tree) :right new-right-subtree))))))

(defun diet-remove (a-val a-tree)
  (cond
    ;; Empty tree - trivial
    ((null a-tree) nil)
    (t
     (let ((bottom (s-diet-node-bottom a-tree))
           (top (s-diet-node-top a-tree))
           (left (s-diet-node-left a-tree))
           (right (s-diet-node-right a-tree)))
       (assert (and (not (null bottom))
                    (not (null top))))
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
      (+ 1
         (diet-height (s-diet-node-right a-tree))
         (diet-height (s-diet-node-left a-tree)))))

(defun diet-balance (a-tree)
  (cond
    ((null a-tree) 0)
    ((and (null (s-diet-node-left a-tree)) (null (s-diet-node-right a-tree))) 0)
    (t (- (diet-height (s-diet-node-right a-tree))
          (diet-height (s-diet-node-left a-tree))))))


(defun diet-max-interval (a-tree)
  (cond
    ((null a-tree) 0)
    ((and (null (s-diet-node-left a-tree)) (null (s-diet-node-right a-tree)))
         (1+ (- (s-diet-node-top a-tree) (s-diet-node-bottom a-tree))))
    ((null (s-diet-node-left a-tree))
     (max (1+ (- (s-diet-node-top a-tree) (s-diet-node-bottom a-tree)))
          (diet-max-interval (s-diet-node-right a-tree))))
    ((null (s-diet-node-right a-tree))
     (max (1+ (- (s-diet-node-top a-tree) (s-diet-node-bottom a-tree)))
          (diet-max-interval (s-diet-node-left a-tree))))
    (t
     (max (diet-max-interval (s-diet-node-left a-tree))
          (diet-max-interval (s-diet-node-right a-tree))))))


