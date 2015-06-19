;;;; Missionaries and cannibals
;;;; A classic / toy AI problem, now solved in Common Lisp.
;;;; When it comes to the problem itself, the Wikipedia has
;;;; quite exhaustive section about it, no need to quip here.
;;;; See for yourself: http://en.wikipedia.org/wiki/Missionaries_and_cannibals_problem
;;;; This file here is just a play with Common Lisp to see and test
;;;; for myself, how much did I learnt from those excellent books we all have
;;;; at our disposal ("PAIP" by Norvig, "PCL" by Siebel, "ANSI CL" & "On Lisp" by Graham)
;;;;
;;;; A bit of explanation how this program works.
;;;; The program tries to move all the missionaries & cannibals from left to right side
;;;; of the river. To do that, program conducts an exhaustive search in the states space.
;;;; A state represents what's left on the left side of the river. A state is represented
;;;; by a P-List, which are keyed with keywords like ':missionaries', ':cannibals' and ':has-boat'.
;;;; So a state:
;;;;   (:missionaries 3 :cannibals 2 :has-boat t :branch-idx 0)
;;;; represents a situation when we have 3 missionaries, 2 cannibals and a boat on the left side of the river.
;;;; To represents subsequent moves, this program operates on the list of such states, called the states stack.
;;;; The last key, :branch-idx, represents how did we get to this state, i.e. which move, among
;;;; 3 possible moves from left to right, and 3 possible moves from right to left, got us where we are.
;;;;
;;;; For instance, a states stack like below:
;;;;   ((:missionaries 2 :cannibals 1 :has-boat nil :branch-idx 2) (:missionaries 3 :cannibals 2 :has-boat t :branch-idx 0))
;;;; represents a situation when we had 3 missionaries and 2 cannibals on the left side,
;;;; and then we have 2 missionaries and 1 cannibal on the left side, with 1 missionary and 1 cannibal
;;;; on the right side. At any moment, the head of the list does represent current state.
;;;;
;;;; As one might have noticed, the states stack is really a stack - we add and remove elements from its front,
;;;; hence the name.
;;;;
;;;; The program is structured so that the easiest parts go first, then a bit harder ones, finally the hardest part.
;;;; Saying 'hardest' is a great overstatement. This is still simple backtracking, so really this is no hard.
;;;; Everywhere I tried to maintain this functional fasion - no hidden variables, limit use of 'setf'.
;;;; This indeed make things simpler to develop and test.
;;;; So the main function backtracking function is 'find-next-move'. It receives current states stack, and searches
;;;; for next possible move.n
;;;; If it does find a next possible move, it adds it to the front of states stack and returns new states stack.
;;;; Otherwise, it returns a modified states stack, with last state removed, and this new last state has
;;;; its :branch-idx argument increased, so that next time we look for a solution, we do not enter this dead alley again.

;;;; 
(in-package :org.ostaszewski.tomasz.missionaries)

(declaim (optimize (speed 3) (safety 0)))

;;; A function that creates a new state with given parameters
(defun new-state (&key missionaries cannibals has-boat (branch-idx 0))
  (declare (fixnum missionaries cannibals branch-idx))
  (list :missionaries missionaries :cannibals cannibals :has-boat has-boat :branch-idx branch-idx))

(defparameter *winning-state* (new-state :missionaries 0 :cannibals 0 :has-boat nil))

(defun make-move (&key missionaries cannibals)
  (declare (fixnum missionaries cannibals))
  (list :missionaries missionaries :cannibals cannibals))

(defun negate-move (a-move)
  (make-move
   :missionaries (- 0 (the fixnum (getf a-move :missionaries)))
   :cannibals (- 0 (the fixnum (getf a-move :cannibals)))))

;;; a list of valid moves from right to left side of the river
(defparameter *valid-moves-r-to-l* (list
                                    (make-move :missionaries 1 :cannibals 1)
                                    (make-move :missionaries 1 :cannibals 0)
                                    (make-move :missionaries 2 :cannibals 0)
                                    ))

;;; a list of valid moves from left to right side of the river
(defparameter *valid-moves-l-to-r* (list
                                    (negate-move (nth 0 *valid-moves-r-to-l*))
                                    (negate-move (nth 1 *valid-moves-r-to-l*))
                                    (negate-move (nth 2 *valid-moves-r-to-l*))))

(defparameter *moves-limit* (1- (length *valid-moves-r-to-l*)))

(defun states-equal-p (lhs-state rhs-state)
  (and (equal (the fixnum (getf lhs-state :missionaries)) (the fixnum (getf rhs-state :missionaries)))
       (equal (the fixnum (getf lhs-state :cannibals))    (the fixnum (getf rhs-state :cannibals)))
       (equal (the fixnum (getf lhs-state :has-boat))     (the fixnum (getf rhs-state :has-boat)))))

(defun winning-state-p (a-state)
  (states-equal-p *winning-state* a-state))

(declaim (inline visited-state-p))
(declaim (inline valid-state-p))

;;; Check if a given state is valid
;;; A state is valid if it does not violate
;;; rules of the game. The rules are:
;;; on neither of the shores of the river,
;;; either left or right, number of cannibals
;;; may be greater than number of missionaries on that shore.
(defun valid-state-p (m c state)
  (declare (fixnum m c))
  (let* ((m-rhs (the fixnum (getf state :missionaries)))
         (c-rhs (the fixnum (getf state :cannibals)))
         (m-lhs (if (< m-rhs m) (- m m-rhs) 0))
         (c-lhs (if (< c-rhs c) (- c c-rhs) 0)))
    (and (<= 0 m-rhs) (<= 0 c-rhs)
         (or (<= c-rhs m-rhs) (eql 0 m-rhs))
         (or (<= c-lhs m-lhs) (eql 0 m-lhs))
         (<= m-rhs m)
         (<= c-rhs c))))

;;; Check if a given state have already been visited
(defun visited-state-p (state states-stack)
  (declare (cons states-stack))
  (find-if #'(lambda(s) (states-equal-p s state)) states-stack))

;;; Returns next possible state
;;; given current state and one of the
;;; possible moves.
(defun get-next-state (m c state-stack move-idx)
  (declare (fixnum move-idx))
  (let ((current-state (first state-stack)))
    (let ((move (nth move-idx *valid-moves-r-to-l*)))
      (when (getf current-state :has-boat)
        (setf move (nth move-idx *valid-moves-l-to-r*)))
      (if move
          (let* ((missionaries (+ (the fixnum (getf current-state :missionaries)) (the fixnum (getf move :missionaries))))
                 (cannibals (+ (the fixnum (getf current-state :cannibals)) (the fixnum (getf move :cannibals))))
                 (has-boat (not (getf current-state :has-boat)))
                 (next-state (new-state :missionaries missionaries :cannibals cannibals :has-boat has-boat)))
            (and (valid-state-p m c next-state) (not (visited-state-p next-state state-stack)) next-state))))))

;;; Finds a valid next state for given current state
;;; and starting branch position.
;;; Starting branch position is an index of the
;;; 'valid-moves' array, which describes all possible valid moves.
;;; If found, returns 2, first of which is the next state
;;; and second is the index of the move taken.
;;; If not found, returns 2 values, first nil, second is the size
;;; of the moves list.
(defun find-next-state (m c states-stack)
  (unless (or (null states-stack) (winning-state-p (first states-stack)))
    (let ((current-state (first states-stack)))
      (when current-state
        (loop
           for move-idx from (the fixnum (getf current-state :branch-idx)) to *moves-limit*
           for next-state = (get-next-state m c states-stack move-idx) then (get-next-state m c states-stack move-idx)
           when next-state return (values next-state move-idx))))))

;;; This returns a new states stack given current one
;;; First 2 parameters are initial number of missionaries and cannibals
;;; on the left side of the river.
(defun solve-next-move (m c states-stack)
  (multiple-value-bind (next-state idx)
      (find-next-state m c states-stack)
    (cond
      ;; No next state
      ;; In that case, return a state stack with last state removed
      ;; After all, from this state we didn't get anywhere.
      ;; Then increase the branch index of this new last state.
      ;; This is done in order for the next try to take another route
      ;; and not to fall into the same pitfall.
      ((null next-state)
       (let ((backtrack (rest states-stack)))
         (when backtrack
           (incf (the fixnum (getf (first backtrack) :branch-idx))))
         backtrack))
      ;; Found a next state
      ;; IN this case set the branch index to the 
      (t (setf (getf (first states-stack) :branch-idx) idx)
         (cons next-state states-stack)))))

;;; Puzzle solver
;;; Returns a list, each element of which is a sequence of moves that lead to a solution.
(defun solve-puzzle (m c)
  (let ((solutions))
    (do ((s-s (list (new-state :missionaries m :cannibals c :has-boat t)) (solve-next-move m c s-s)))
        ((or (not s-s)) nil)
      (when (winning-state-p (first s-s))
        (push (nreverse (copy-tree s-s)) solutions)))
    (nreverse solutions)))
