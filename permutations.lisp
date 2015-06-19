#| 
A simple iterative permutations generator

This generator performs an iterative depth search of the permutations tree. Here's how it goes, 
given an example set with just 2 (two) elements. With higher set cardinalities it goes the same. 

So we have a root of this tree, called "null" (just like the empty set, "crossed 0"). 
From this root we have two branches stemming out, labeled "0" and "1".
We start our journey at the root, and we move along branches. We follow a branch
labeled with "0" and end up in a node labeled "0", arriving at tree level 1. From this node we
have 2 more branches spanning out, both labelled with "0" and "1". But now, the branch labelled with
"0" leads is a dead end, labelled with "null". This is because we already have "0" in our permutation,
and permutations do not have repeated elements. If we then follow a branch labelled with "1", we end
up in a node labelled with "1", at tree level 2. There won't be more branches spanning from this
node, since we are already at a level that is equal to the cardinality of our set. And our first
permutation is "01", which can be read from values of either branches or nodes along the road to
this node. So we go back to level 1, and since there's no more branches to take, we go back to level
0. This time we start with branch labelled "1", and the story pretty much repeats there. We reach
level 2 via chain "10", arriving at our second and final permutation "10". 

Below is a pictorial demonstration:

"null"
|
|
*-- branch 0 -- "0" -- branch 0 -- "null"
|                |
|                *-- branch 1 -- "1" 
|
*-- branch 1 -- "1" -- branch 0 -- "0"
|
*-- branch 1 -- "null" 

This implementation does not use a real tree, it doesn't need it. It merely uses a table,
'perm-table', that holds current permutation set, and an extra variable, 'tree-level', that tells
how far/deep along the road are we. A main loop adds elements to current permutation, minding that
there are no repetitions. Main loop exits if one of the 2 possible conditions is fulfilled:
* there's a complete permutation.
In that case a complete permutation is returned. This is a copy of the current state of the
permutation array.
* there are _NO_MORE_ permutations to complete.
In that case a nil is returned.

|#

                                        ;(defpackage :
                                        ;(in-package :org.ostaszewski.tomasz.permutations)

;;(declaim (optimize (speed 3) (safety 0)))

(defstruct permutator
  perm
  is-leaf)

;; Private method
;; Finds next possible candidate
;; Perm - current permutation vector
;; idx - index of an element that we are about to fix
;; This function finds a value for the the next element
;; after last fixed element.
;; If such a value cannot be found, it returns nil.
(defun perm-find-next (vec-curr-perm level branch-idx)
  ;; Local macro not-visited-p
  ;; A predicate that checks if a given element 'state' does not
  ;; occur in the sequence 'seq' up to index 'idx'.
  ;; This is just syntactic sugar over standard LISP functions.
  ;; Returns 't' if it doesn't, returns 'nil' if it does.
  (macrolet ((not-visited-p (state seq idx)
               `(not (find ,state ,seq :end ,idx))))
    ;; Loop over all the states, starting form the one 
    ;; at the head of the 'perm-table' vector, and 
    ;; ending at the very last possible state. 
    ;; For each state, check if it is possible to make
    ;; a move, i.e. if a state being contemplated 
    ;; is not the one we already took. That's why
    ;; we call 'state-visited-p' helper.
    (loop
       for idx from branch-idx to (1- (length vec-curr-perm))
       do (if (not-visited-p idx vec-curr-perm level)
              (return-from nil idx))
       finally
         (return nil))))

;;
(defun get-next-perm (a-vec a-level a-idx)
  (let ((vec (copy-seq a-vec)))
    ;; Helper local function.
    ;; I find it more natural, in this particular case
    ;; to use recursion rather than a loop, especially
    ;; that in this case it nicely reduces itself to tail-recursion.
    (labels ((rec (level idx)
               (let ((next-state (perm-find-next vec level idx)))
                 (cond
                   ;; A next item of the permutation is successfully found.
                   ;; In that case add this item to the permutations and advance
                   ;; to the next tree level.
                   (next-state
                    (setf (svref vec level) next-state)
                    ;; Have we reached the a complete permutation? If so, return it.
                    (if (eql (array-total-size vec) (1+ level))
                        vec
                        ;; Otherwise, advance to the next position,
                        ;; starting from 0 branch
                        (rec (1+ level) 0)))
                   (t 
                    ;; Have we run out of possible states?
                    ;; Return a nil permutation in this case.
                    (when (eql 0 level)
                      (return-from rec nil))
                    ;; Otherwise - back up to the previous tree level.
                    (rec (1- level) (1+ (svref vec (1- level)))))))))
      ;; Call a recursive function to determine
      ;; a next permutation
      (rec a-level a-idx))))

(defun itf-get-next-perm (a-perm)
         ;; tree-level is a current level of the tree
         ;; When we start at root, it is level 0.
         ;; Then, after each permutation, we land at a leaf
         ;; and the level = permutation size
         ;; The member 'is-leaf' tells whether we are
         ;; at a leaf or in the root of the tree.
         ;; branch-idx is an index of a branch we took last
         ;; This only matters if we are at the leaf of the tree.
         ;; In that case it is the index of the last element
         ;; of the permutation.
         ;; When at the root of the tree, then we start from 0.
  (multiple-value-bind (tree-level branch-idx)
      (let ((len (length (permutator-perm a-perm))))
       (if (permutator-is-leaf a-perm)
          (values len (svref (permutator-perm a-perm) (1- len)))
          (values 0 0)))
    (let 
        ;; Finally, we obtain a new permutation
        ((new-perm (get-next-perm (permutator-perm a-perm) 
                    tree-level
                    branch-idx)))
      (if (null new-perm)
          ;; No next permutation - go back to the root
          ;; of the tree and return nil
          (setf (permutator-is-leaf a-perm) nil)
          ;; There is a next permutation
          ;; Change the permutator to reflect that
          ;; and return this last permutation
          (setf (permutator-is-leaf a-perm) t
                (permutator-perm a-perm) new-perm)))))

;;;
(defun create (set-size)
  (declare (fixnum set-size))
  (make-permutator
   :perm (make-array set-size :element-type 'signed-byte)
   :is-leaf nil))

