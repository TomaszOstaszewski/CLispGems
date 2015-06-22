;;
(defparameter *labirynth*
  '((1 . 5) (5 . 9) (9 . 13) (13 . 14) (14 . 15)
    (14 . 10) (10 . 6) (6 . 2) (2 . 3) (3 . 4)
    (4 . 8) (8 . 12) (12 . 11) (11 . 7) (12 . 16)))

;;
(defun single (lst)
  (and (consp lst) (not (cdr lst))))

;;
(defmacro get-candidates-list (room-idx maze)
  `(gethash ,room-idx ,maze))

;; A macro that facilitates maze creation
;;
(defun add-route(from to maze)
  (setf (gethash from maze) (cons to (gethash from maze))))

(defmacro room-not-visited-p (room-idx moves-stack)
  `(not (assoc ,room-idx ,moves-stack)))

;; Create a maze given the labirynth.
;; A labirynth is an assoc list, with each cons of which
;; describing the way from one maze room to another.
;; So a maze like this:
;; *---*
;; |1|2|
;; |3 4|
;; *---*
;; has a corresponding labirynth (assoc list) given below:
;; ((1 . 3) (3 . 4) (4 . 2))
;; A maze, on the other hand, is a hash table, whose key is a room index
;; and value is a list of rooms directly connected to it.
(defun create-maze (lab)
  (let ((maze (make-hash-table)))
    (mapcar #'(lambda (x)
               (let ((u (car x)) (v (cdr x)))
                  (add-route u v maze)
                  (add-route v u maze))) lab)
    maze))


;; Find a next room to which one can move form the current room.
;; This next room is not visited, function starts with room index
;; equal to 'branch-idx'
(defun get-next-room (curr-room branch-idx visited maze)
  (let ((all-candidates (get-candidates-list curr-room maze)))
    (loop
       for candidate-room in (nthcdr branch-idx all-candidates)
       for idx from branch-idx
       do (if (room-not-visited-p candidate-room visited)
              (return (values candidate-room idx)))
       finally
         (return (values nil branch-idx)))))

;;
(defun solve-maze (maze start-room-idx test)
  (let* ((curr-branch 0) ; Index of the branch we now take
       ;; States stack. This assoc list holds
       ;; states, composed of both current room number
       ;; and current door index.
       ;; - car contains a room number,
       ;; - cdr contains door index, also called branch number.
       ;;   This says by which door, from  a previous room, did we get here.
       (moves-stack (list (cons start-room-idx curr-branch))))
    ;; Syntactic sugar macro
    ;; Current state is always in the head of the moves stack
    ;; Current room is the car of this head
    (symbol-macrolet ((curr-room (car (car moves-stack))))
      (loop
         until (funcall test curr-room)
         do
           (multiple-value-bind (next-room next-branch)
               (get-next-room curr-room curr-branch moves-stack maze)
             (cond
               ;; A valid next room
               ;; Move to it and push it on the moves stack
               (next-room
                (format t "~&~vt~a~%" (length moves-stack) next-room)
                (push (cons next-room next-branch) moves-stack)
                ;; Start over branching
                (setf curr-branch 0))
               ;; Dead corner and nowhere to backtrack,
               ;; - bail out
               ((single moves-stack)
                (pprint (list "Unsolved" moves-stack))
                (return-from nil nil))
               ;; Dead corner with a backtrack,
               ;; move back - pop current state form stack
               ;; and start from next branch after previously taken
               (t (setf curr-branch (1+ (cdr (pop moves-stack))))
                  (format t "~&~vt~a~%" (1- (length moves-stack)) curr-room))))
         finally (return (nreverse (mapcar #'car moves-stack)))))))
