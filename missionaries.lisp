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
;;;; For instance, a state stack like below:
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
;;;; I tried to maintain a functional fashion - no hidden variables, limit use of 'setf'.
;;;; This indeed makes things simpler to develop and test.
;;;; So the main backtracking function is 'find-next-move'. It receives current states stack, and searches
;;;; for a next possible move.
;;;; If it finds one, it will add it to the front of states stack and returns new states stack.
;;;; Otherwise, it returns a modified states stack, with last state removed, and this new last state has
;;;; its :branch-idx argument increased, so that next time we look for a solution, we do not enter this dead alley again.

;;;; 
;(in-package :org.ostaszewski.tomasz.missionaries)

;;(declaim (optimize (speed 3) (safety 0)))

;; A winning state is the one when we have
;; no missionaries, no cannibals nad no boat
;; on the left side.
(defparameter *winning-state* `(:missionaries 0 :cannibals 0 :has-boat nil))

(defvar *m*)
(defvar *c*)

;; A list of valid moves.
;; We have 2 types of moves;
;; - with boat on the left side;
;;   In that case, only valid moves are to take 2 missionaries,
;;   a missionary and a cannibal or just a missionary form the left side.
;; - without boat on the left side;
;;   Just like above, but we take from the right side.
;; 
(defparameter *valid-moves*
  `(:without-boat ((:missionaries 1 :cannibals 1)
                   (:missionaries 1 :cannibals 0)
                   (:missionaries 2 :cannibals 0))
     :with-boat ((:missionaries -1 :cannibals -1)
                 (:missionaries -1 :cannibals 0)
                 (:missionaries -2 :cannibals 0))))

;; A function that compares 2 states
(defun states-equal-p (lhs-state rhs-state)
  (and (equal (getf lhs-state :missionaries) (getf rhs-state :missionaries))
       (equal (getf lhs-state :cannibals) (getf rhs-state :cannibals))
       (equal (getf lhs-state :has-boat) (getf rhs-state :has-boat))))

;; A function that checks if a given state is the end state
(defun winning-state-p (a-state)
  (states-equal-p *winning-state* a-state))

(defmacro new-state (&rest rest)
  `(list ,@rest :branch-idx 0))

(declaim (inline visited-state-p))
(declaim (inline valid-state-p))
(declaim (inline winning-state-p))

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
  (find-if #'(lambda(s) (states-equal-p s state)) states-stack))

;;; Returns next possible state
;;; given current state and one of the
;;; possible moves.
(defun get-next-state (state-stack move-idx)
  (let* ((current-state (first state-stack))
         (move (nth move-idx
                    (if (getf current-state :has-boat)
                        (getf *valid-moves* :with-boat)
                        (getf *valid-moves* :without-boat)))))
    (if move
        (let ((missionaries (+ (getf current-state :missionaries) (getf move :missionaries)))
              (cannibals (+ (getf current-state :cannibals) (getf move :cannibals)))
              (has-boat (not (getf current-state :has-boat))))
          (let ((next-state (new-state :missionaries missionaries :cannibals cannibals :has-boat has-boat)))
            (and (valid-state-p *m* *c* next-state) (not (visited-state-p next-state state-stack)) next-state))))))

;;; Finds a valid next state for given current state
;;; and starting branch position.
;;; Starting branch position is an index of the
;;; 'valid-moves' array, which describes all possible valid moves.
;;; If found, returns 2, first of which is the next state
;;; and second is the index of the move taken.
;;; If not found, returns 2 values, first nil, second is the size
;;; of the moves list.
(defun find-next-state (states-stack)
  (unless (or (null states-stack) (winning-state-p (first states-stack)))
    (let ((current-state (first states-stack)))
      (when current-state
        (loop
           for move-idx from (getf current-state :branch-idx) to (1- (length (getf *valid-moves* :with-boat)))
           for next-state = (get-next-state states-stack move-idx) then (get-next-state states-stack move-idx)
           when next-state return (values next-state move-idx))))))

;;; This returns a new states stack given current one
;;; First 2 parameters are initial number of missionaries and cannibals
;;; on the left side of the river.
(defun solve-next-move (states-stack)
  (multiple-value-bind (next-state idx)
      (find-next-state states-stack)
    (cond
      ;; No next state
      ;; In that case, return a state stack with last state removed
      ;; as this last state did lead us to a dead end.
      ;; But before doint so, increase the branch index of a predecessor of the last state.
      ;; This is done in order to try another route next time we search
      ;; for a valid move.
      ((null next-state) (let ((backtrack (rest states-stack)))
                           (when backtrack
                             (incf (getf (first backtrack) :branch-idx)))
                           backtrack))
      ;; Found a next state
      ;; In that case set the branch index to the
      ;; index of a branch taken and return new states stack
      (t (setf (getf (first states-stack) :branch-idx) idx)
         (cons next-state states-stack)))))

;;; Puzzle solver
;;; Returns a list, each element of which is a sequence of moves that lead to a solution.
(defun solve-puzzle (m c)
  (let ((*m* m)
        (*c* c))
    (let ((solutions))
      (loop for s-s = (list (new-state :missionaries m :cannibals c :has-boat t))
         then (solve-next-move s-s)
         until (null s-s)
         when (winning-state-p (car s-s)) do (push (nreverse (copy-tree s-s)) solutions))
      (nreverse solutions))))
