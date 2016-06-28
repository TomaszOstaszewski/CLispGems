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

;; Walks a tree constantly to the right and calls a collector function
;; with two paramters:
;; - a value of the maximum node - a node with no right child
;; - a tree without this maximum node
(defun split-max (a-tree collector)
  (if (null a-tree)
      (funcall collector nil nil)
      (with-slots (val left right) a-tree
        (if (null right)
            (funcall collector val left)
            (split-max right
                       #'(lambda(last-val new-tree)
                           (funcall collector last-val (make-bst-node :val val :left left :right new-tree))))))))

;; Walks a tree constantly to the left and calls a collector function
;; with two paramters:
;; - a value of the minimum node - a node with no left child
;; - a tree without this minimum node
(defun split-min (a-tree collector)
  (if (null a-tree)
      (funcall collector nil nil)
      (with-slots (val left right) a-tree
        (if (null left)
            (funcall collector val right)
            (split-max left
                       #'(lambda(last-val new-tree)
                           (funcall collector last-val (make-bst-node :val val :left new-tree :right right))))))))

;; Knuth's delete by copying
;; When a node is to be deleted, we find its successor/predecessor
;; Then we create a new node, with this successor's value
;; and left/rigth subtree set to the successor's left/right subtree
;; and right/left subtree set to this node's right/left subtree.
(defun bst-delete (a-val a-root)
  (if (null a-root)
      nil
      (with-slots (val left right) a-root
        (cond
          ((< a-val val) (make-bst-node :val val :left (bst-delete a-val left) :right right))
          ((> a-val val) (make-bst-node :val val :left left :right (bst-delete a-val right)))
          (t (if (zerop (random 2))
                 ; Replace a node with its predecessor
                 (split-max left
                            #'(lambda (new-root-val max-left-subtree)
                                (make-bst-node :val new-root-val :left max-left-subtree :right right)))
                 ; Replace a node with its successor
                 (split-min right
                            #'(lambda (new-root-val min-right-subtree)
                                (make-bst-node :val new-root-val :left left :right min-right-subtree)))))))))

(defun bst-preorder (a-root fn)
  (when a-root
    (with-slots (val left right) a-root
      (funcall fn val)
      (bst-preorder left fn)
      (bst-preorder right fn))))

(defun bst-inorder (a-root fn)
  (when a-root
    (with-slots (val left right) a-root
      (bst-preorder left fn)
      (funcall fn val)
      (bst-preorder right fn))))


(defun bst-postorder (a-root fn)
  (when a-root
    (with-slots (val left right) a-root
      (bst-preorder left fn)
      (bst-preorder right fn)
      (funcall fn val))))
