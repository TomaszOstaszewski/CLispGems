(defstruct bst-node val (left nil) (right nil))

(defun bst-insert (a-val a-root)
  (if (null a-root)
      (make-bst-node :val a-val :left nil :right nil)
      (with-slots (val left right) a-root
        (cond
          ((< a-val val) (make-bst-node :val val :left (bst-insert a-val left) :right right))
          ((> a-val val) (make-bst-node :val val :left left :right (bst-insert a-val right)))
          (t a-root)))))

(defun bst-member? (a-val a-root)
  (if (null a-root)
      nil
      (with-slots (val left right) a-root
        (cond
          ((< a-val val) (bst-member? a-val left))
          ((> a-val val) (bst-member? a-val right))
          (t t)))))


(defun split-max (a-tree collector)
  (if (null a-tree)
      (funcall collector nil nil)
      (with-slots (val left right) a-tree
        (if (null right)
            (funcall collector val left)
            (split-max right
                       #'(lambda(last-val new-tree)
                           (funcall collector last-val (make-bst-node :val val :left left :right new-tree))))))))

(defun split-min (a-tree collector)
  (if (null a-tree)
      (funcall collector nil nil)
      (with-slots (val left right) a-tree
        (if (null left)
            (funcall collector val right)
            (split-max left
                       #'(lambda(last-val new-tree)
                           (funcall collector last-val (make-bst-node :val val :left new-tree :right right))))))))

(defun bst-delete (a-val a-root)
  (if (null a-root)
      nil
      (with-slots (val left right) a-root
        (cond
          ((< a-val val) (make-bst-node :val val :left (bst-delete a-val left) :right right))
          ((> a-val val) (make-bst-node :val val :left left :right (bst-delete a-val right)))
          (t (if (zerop (random 2))
                 (split-max (bst-node-left a-root)
                            #'(lambda (new-root-val max-left-subtree)
                                (make-bst-node :val new-root-val :left max-left-subtree :right right)))
                 (split-min (bst-node-right a-root)
                            #'(lambda (new-root-val min-right-subtree)
                                (make-bst-node :val new-root-val :left left :right min-right-subtree)))))))))
