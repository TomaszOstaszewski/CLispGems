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
          :bottom (s-diet-node-right a-diet-node)
          :top (s-diet-node-top a-diet-node)
          :left (s-diet-node-left a-diet-node)
          :right (diet-insert value (s-diet-node-right a-diet-node)))))
    (t a-diet-node)))

(defun all-but-leftmost-leaf (a-tree)
  (cond 
    ((null a-tree) nil)
    ((null (second a-tree)) (values (first a-tree) nil))
    (t (multiple-value-bind (node rest)
           (all-but-leftmost-leaf (second a-tree))
         (values node (list (first a-tree) rest (third a-tree)))))))
