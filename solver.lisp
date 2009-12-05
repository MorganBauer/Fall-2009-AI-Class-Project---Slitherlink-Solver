(in-package #:slither)

;; -mhb
;;;; ACHTUNG!!! ATTIENTION!
;;
;;
;;    We need to standardize on some calling features
;;    I keep trying to use your functions,
;;    but they have weird formats
;;    thus I propose that in function signatures we have:
;;
;;    - board, because that is the most important thing,
;;          followed by the coords
;;    - x, followed by
;;    - y
;;
;;    This is pretty standard for DS access I believe...
;;
;;    Goddamnit dana this is so fucked up. It's solves 90 degress off...
;;       but doesn't solve everything...
;;
;;  Also, it's two semicolons for a comment that is poperly indented
                                        ; single colons auto indend to way out
                                        ; here when they are on single lines
;; you can type 'M-;' (Alt-Semicolon) to have it type the correct one for you.
;;
;; Do (run-tests) to see unit tests run. Please write your own if you wish.
;; -mhb







;; (defconstant +up+ (list #'1- 'x))
;; left y-
;; right y+
;; down x+


;; Upon furthur review, I believe it would be simplest to dfs based on the points.
;; this is not what I have below, but something similar follows from what I have below. Do an 'is-point?' function instead to check where points are, then simply dfs from the direction that is not already occupied. if there are more than two edges leading away from the current point, return failure, so that we backtrack.
;; -mhb
;;;;

;;also, almost certainly we can use a macro at some point for shortening stuff up, but probably don't need too.


                                        ;(declaim (inline is-it-a-line-position?))

(defun solve (board)
  (let ((starting-time (get-universal-time)) ; remember starting time
        (ending-time nil)
        (moves nil)) ; create empty list for moves
    ;; when dfs'ing on dots, we can go from 0 to (1- dimension-limit) by 2
    ;; Perhaps instead of picking an arbitrary starting position,
    ;; start at one of the squares that
    ;; actually contains a number other than zero
    ;; (therefore one of those four spaces has to have a line)
    ;; That way we avoid marking a place where there may not need a line at all.
    ;;
    ;; mhb-- This should preferably be passed in to another function
    ;; rather than just being used for it's side effects, but I do not
    ;; think that we need the original board after this, so it is okay.
    (apply-x-marks board)

    ;; (find-a-plausible-starting-location-then-dfsolve) ; see below

    (loop for x fixnum from 0 to (array-dimension board +X+) by 2
       :do (loop for y fixnum from 0 to (array-dimension board +Y+) by 2
              ;; make a function pass it in.
              do (progn
                   (format t "~&dfs at x:~A,y:~A~&" x y)
                   (if (dfs board x y moves) ; if it returns true, we have found a solutio
                       (progn
                         (setf ending-time (get-universal-time))
                         (format t "we have found the solution")
                         (print-board board)
                         (print-moves (nreverse moves))
                         (format t "~&Solve took ~D seconds" (- ending-time
                                                                starting-time))
                         t))))))
                                        ;(format t "~&holy moly, there is no solution!~%This should not be possible!~&")
  )

;; (defmacro loop-over-vertexes (board)
;;   (loop for x fixnum from 0 to (array-dimension board +X+) by 2
;;      :do (loop for y fixnum from 0 to (array-dimension board +Y+) by 2
;; (defmacro loop-over-faces (board)
;;   (loop for x fixnum from 1 to (1- (array-dimension board +X+)) by 2
;;      :do (loop for y fixnum from 1 to (1- (array-dimension board +Y+)) by 2

;;;; Apply the x marks, only apply where we can be sure!!!
(defun apply-x-marks (board)
  (declare (type array board))
  (loop for x fixnum :from 1 :to (1- (array-dimension board +X+)) by 2
     :do (loop :for y fixnum from 1 to (1- (array-dimension board +Y+)) by 2
            ;; might need many more cases, but let's not go crazy...
            ;; ? :when (integerp (aref board x y)) to explicitly ignore spaces
            do (if (eql (aref board x y) 0) ; around zeroes
                   (setf (aref board x (1+ y)) #\X
                         (aref board x (1- y)) #\X
                         (aref board (1+ x) y) #\X
                         (aref board (1- x) y) #\X))))
  board)


;; mhb--
;; I think this should return a value of coordinates for dfs to start at.
;; Other extra goodies to help the solver speed up are welcome,
;; but should be put within the dfs itself, or before it
;; in something like the apply-x-marks (which may need to be renamed
;; as I can think of many things to add that are no x's)
;; --mhb
(defun find-a-plausible-starting-location-then-dfsolve (board moves)
  ;; A good starting location will be a cross that has a higher number on it.
  ;; if the number is a three, then EVERY + will guarantee a solution.
  ;; at the most, if there are no 3's, then a 2 or 1 will suffice.
  ;; Then, worst case scenario, two dfs's will have to be done,
  ;; starting from opposite corners.
  (let ((start-pt-x nil)
        (start-pt-y nil))
                                        ; Check for 3's on the board
    (loop for x fixnum from 1 to (- (array-dimension board +X+) 2) by 2
       :do (loop for y fixnum from 1 to (- (array-dimension board +Y+) 2) by 2
              :do (if (typep (aref board x y) 'integer)
                      (if (= 3 (aref board x y))
                          (progn
                            (multiple-value-setq (start-pt-x start-pt-y) (values x y))
                            (return))
                          ())
                      ())
                )
         (if (typep start-pt-x 'integer) (return) () )
         )
                                        ; only check for twos if there are no threes.
                                        ; Will skip over this if start-pt-x is not nil
    (if (typep start-pt-x (type-of nil)) ; checks by type so there are no errors...
        (loop for x fixnum from 1 to (array-dimension board +X+) by 2
           :do (loop for y fixnum from 1 to (array-dimension board +Y+) by 2
                  :do (if (typep (aref board x y) 'integer)
                          (if (= 2 (aref board x y))
                              (progn
                                (multiple-value-setq (start-pt-x start-pt-y) (values x y))
                                (return))
                              ())
                          ())
                    )
             (if (typep start-pt-x 'integer) (return) () )
             ) () ) ; else does nothing...
    (if (typep start-pt-x (type-of nil)) ; checks by type so there are no errors...
        (loop for x fixnum from 1 to (array-dimension board +X+) by 2
           :do (loop for y fixnum from 1 to (array-dimension board +Y+) by 2
                  :do (if (typep (aref board x y) 'integer)
                          (if (= 1 (aref board x y))
                              (progn
                                (multiple-value-setq (start-pt-x start-pt-y) (values x y))
                                (return))
                              ())
                          ()) ; hooray for nested ifs inside nested loops... </sarcasm>
                    )
             (if (typep start-pt-x 'integer) (return) () )) () ) ; elses tend to do nothing...
    ;; debug line: outputs the x and y of the starting box [which line is debug? --mhb]
    (values start-pt-x start-pt-y)
    ;; So unless the board was completely blank, start-pt-x and start-pt-y should have a
    ;; square with a number in it.  It favors 3s over 2s, and 2s over 1s.  It overlooks spaces
    ;; and zeroes.
    ;; DFS from the upper-left corner of the number...
    (if (dfs (- start-pt-x 1) (- start-pt-y 1) board moves)
        (zomg-hax-win board)
        ;; else (ie you don't win) DFS from the bottom-right corner...
        (if (dfs (+ start-pt-x 1) (+ start-pt-y 1) board moves)
            (zomg-hax-win board) ()))))

;; Are we having fun yet?
;; I wish to do this recursively. It may make the code a little easier, but perhaps
;; make the process slower...


(defun dfs (board x y moves)
  (declare (type fixnum x y)
           (type array board)
           (type list moves))
  ;(format t "~5%")
  (if (check-board board)
      (return-from dfs board)
      (progn
        (and board x y moves))))

(define-test dfs
  (let ((solution (read-board-solution "game1solution.txt"))
        (moves nil))
    ;; Sanity-check to ensure that the checker actually checks.
    (assert-true (check-board solution))
    (assert-true (check-board (dfs solution 0 0 moves)))
    (assert-false moves)
    ;; Solution with full hints
    (setf solution (read-board-solution "game2solutionFilled.txt"))
    (assert-true (check-board (dfs solution 0 0 moves)))
    (assert-false moves)
    ;; solution with no hints
    (setf solution (read-board-solution "game2solution.txt"))
    (assert-true (check-board (dfs solution 0 0 moves)))
    (assert-false moves)
    ;; Filled board
    (setf solution (read-board "game2filled.txt"))
    (assert-true (check-board (dfs solution 0 0 nil)))
    ;; Normal board
    (setf solution (read-board "game2.txt"))
    (assert-true (check-board (dfs solution 0 0 nil)))))

(define-test direction-checking
  (let ((no-direction-possible (make-array '(3 3)
                                           :initial-contents (list " | "
                                                                   "-+-"
                                                                   " | "))))
    (assert-false (check-direction no-direction-possible 0 1))
    (assert-false (check-direction no-direction-possible 1 0))
    (assert-false (check-direction no-direction-possible 0 2))
    (assert-false (check-direction no-direction-possible 2 0))))

(defun check-direction (board x y)
  (and board x y))


(defun allowable-line-x (board x)
  (declare #.*common-optimization-settings*
           (type fixnum x) (type array board))
  (if (AND (>= x 0)
           (<= x (1- (array-dimension board +X+))))
      t nil))

(defun allowable-line-y (board y)
  (declare #.*common-optimization-settings*
           (type fixnum y) (type array board))
  (if (AND (>= y 0)
           (<= y (1- (array-dimension board +Y+))))
      t nil))

;; (defun dfs (board x y moves depth)
;;   (declare (type array board))
;;   ;; (format t "~5&")
;;   (if (> depth 60)
;;       (progn
;;         (print "returning depth exceeded")
;;         (return-from dfs nil))
;;       (if (check-board board) ; The board is solved
;;           (progn (format t "~10%HOORAY~10%")
;;                  (return-from dfs t))
;;           (if (valid-node board x y)
;;               (progn
;;                 ;; (format t "node is valid, X=~d,Y=~D~&" x y)
;;                 ;; (format t "current depth = ~D~& board before moving :" depth)
;;                 ;; (print-board board)
;;                 ;; (format t "Vertex Coords X=~2d,Y=~2D~&" x y)
;;                 ;;;; DFS section Start dfs here

;;                 ;; down (+x)
;;                 (let ((x (1+ x))
;;                       (y y))
;;                   ;; (format t "check down")
;;                   ;; (format t "    X=~2d,Y=~2D~&" x y)
;;                   (if (and (allowable-line-x board x)
;;                            (not (is-line x y board)))
;;                       (progn
;;                         ;; (format t " X=~d,Y=~D & going down ~&" x y)
;;                         (setf (aref board x y) #\|)
;;                         (push (cons x y) moves)
;;                         (if (dfs board (1+ x) y moves (1+ depth))
;;                             (return-from dfs t)
;;                             (progn
;;                               (pop moves)
;;                               (setf (aref board x y) #\Space)
;;                               ;; (format t "ALERT")
;;                               ;; (print-board board)1
;;                               ;; (format t "ALERT")
;;                               ;(return-from dfs nil)
;;                               )))))

;;                 ;; up
;;                 (let ((x (1- x))
;;                       (y y))
;;                   ;; (format t "check up")
;;                   ;; (format t "      X=~2d,Y=~2D~&" x y)
;;                   (if (and (allowable-line-x board x)
;;                            (not (is-line x y board)))
;;                       (progn
;;                         ;; (format t " X=~d,Y=~D & going up ~&" x y)
;;                         ;; (format t "  is-line = ~A ~&" (is-line x y  board))
;;                         ;; (format t "  a-linex = ~A ~&" (allowable-line-x board x))
;;                         ;; (format t "  a-liney = ~A ~&" (allowable-line-y board y))
;;                         ;; (format t "     char = ~A ~&" (aref board x y))
;;                         (setf (aref board x y) #\|)
;;                         (push (cons x y) moves)
;;                         (if (dfs board (1- x) y moves (1+ depth))
;;                             (return-from dfs t)
;;                             (progn
;;                               (pop moves)
;;                               (setf (aref board x y) #\Space)
;;                               ;(return-from dfs nil)
;;                               )))))

;;                 ;; right
;;                 (let ((x x)
;;                       (y (1+ y)))
;;                   ;; (format t "check right")
;;                   ;; (format t "      X=~2d,Y=~2D~&" x y)
;;                   (if (and (allowable-line-y board y)
;;                            (not (is-line x y board)))
;;                       (progn
;;                         ;; (format t " X=~d,Y=~D & going right ~&" x y)
;;                         ;; (format t "  is-line = ~A ~&" (is-line x y  board))
;;                         ;; (format t "  a-linex = ~A ~&" (allowable-line-x board x))
;;                         ;; (format t "  a-liney = ~A ~&" (allowable-line-y board y))
;;                         ;; (format t "     char = ~A ~&" (aref board x y))
;;                         (setf (aref board x y) #\-)
;;                         (push (cons x y) moves)
;;                         (if (dfs board x (1+ y) moves (1+ depth))
;;                             (return-from dfs t)
;;                             (progn
;;                               (pop moves)
;;                               (setf (aref board x y) #\Space)
;;                               ;(return-from dfs nil)
;;                               )))))



;;                 ;; left
;;                 (let ((x x)
;;                       (y (1- y)))
;;                   ;; (format t "check left")
;;                   ;; (format t "      X=~2d,Y=~2D~&" x y)
;;                   (if (and (allowable-line-y board y)
;;                            (not (is-line x y board)))
;;                       (progn
;;                         ;; (format t " X=~d,Y=~D & going left ~&" x y)
;;                         ;; (format t "  is-line = ~A ~&" (is-line x y  board))
;;                         ;; (format t "  a-linex = ~A ~&" (allowable-line-x board x))
;;                         ;; (format t "  a-liney = ~A ~&" (allowable-line-y board y))
;;                         ;; (format t "     char = ~A ~&" (aref board x y))
;;                         (setf (aref board x y) #\-)
;;                         (push (cons x y) moves)
;;                         (if (dfs board x (1- y) moves (1+ depth))
;;                             (return-from dfs t)
;;                             (progn
;;                               (pop moves)
;;                               (setf (aref board x y) #\Space)
;;                               ;(return-from dfs nil)
;;                               )))))




;;                 ;(format t "ran out of moves to make, returning...")
;;                 nil
;;                 )))))



;; (defun is-line-solver (x y board)
;;   (declare #.*common-optimization-settings*
;;            (type fixnum x y))
;;   "Pass in the position in the array
;; If it is a line, return a 1"
;;   ;; (format t "~&is-line!!!~&")
;;   (if (and (allowable-line-x board x)
;;            (allowable-line-y board y))
;;       (let ((char (aref board x y)))
;;         (declare (type character char))
;;         ;(format t "~@:c" char)
;;         (case char
;;           (#\| t)
;;           (#\- t)
;;           (#\Space nil)
;;           (#\X nil)))
;;       nil))

;; (defun dfs (board x y moves depth) ; maybe add a depth so that we don't get too far in?
;;   (declare (type array board))
;;   (format t "~%~%before moving : depth = ~D%" depth)
;;   (print-board board)
;;   ;; (format t "~A~&" (check-board board))
;;   (if (> depth  10)
;;       (return-from dfs nil)
;;       (if (check-board board) ; The board is solved
;;           (return-from dfs t)
;;           (if (valid-node board x y)
;;               (progn
;;                 (format t "node is valid~&")
;;                 ;; down
;;                 (if (and (allowable-line-x board (1+ x)) (not (is-line (1+ x) y  board))
;;                          (not (format t "can go dn? ~A~&" (not (is-line (1+ x) y board)))))
;;                     (progn

;;                       (format t "~&down! @ ~D,~D" x y)
;;                       (format t " from is-line = ~A ~&" (is-line (1- x) y  board))
;;                       (push (cons (1+ x) y) moves)
;;                       (setf (aref board  (1+ x) y) #\|)
;;                       (if (dfs board (1+ (1+ x)) y moves (1+ depth))
;;                           (return-from dfs t)
;;                           (progn
;;                             (format t "POP")
;;                             (format t "~Dx~Dy" x y)
;;                             (pop moves)
;;                             ;; (print-board board)
;;                             (format t "   ~:C~&" (aref board (1+ x) y))
;;                             (setf (aref board (1+ x) y) #\Space)
;;                             (return-from dfs nil))))
;;                     nil)
;;                 ;;left

;;                 (if (and (allowable-line-y board (1- y)) (not (is-line x (1- y) board))
;;                          (not (format t "can go left? ~A" (not (is-line x (1- y)  board ))))
;;                          (not (format t "  @ board is ~C" (aref board x (1- y))))
;;                          (not (format t " with c= ~A~&" (char= (aref board x (1- y)) #\-)))
;;                          (char= (aref board x (1- y)) #\-))
;;                     (progn

;;                       (format t "~&left! @ ~D,~D" x y)
;;                       (format t "~c" (aref board x (1- y)) )
;;                       (format t " from is-line = ~A ~&" (is-line x (1- y)  board))
;;                       (push (cons x (1- y)) moves)
;;                       (setf (aref board  x (1- y)) #\-)
;;                       (if (dfs board (1- (1- x)) y moves (1+ depth))
;;                           (return-from dfs t)
;;                           (progn
;;                             (format t "POP")
;;                             (format t "~Dx,~Dy" x y)
;;                             (pop moves)
;;                             ;; (print-board board)
;;                             (format t "   ~:C~&" (aref board x (1- y)))
;;                             (setf (aref board x (1- y)) #\Space)
;;                             (return-from dfs nil))))
;;                     nil)


;;                 ;;right
;;                 (if (and (allowable-line-y board (1+ y)) (not (is-line x (1+ y) board))
;;                          (not (format t "can go rt? ~A" (not (is-line x (1+ y)  board ))))
;;                          (not (format t "  @ board is ~C" (aref board x (1+ y))))
;;                          (not (format t " with c= ~A~&" (char= (aref board x (1+ y)) #\-)))
;;                          (char= (aref board x (1+ y)) #\-))
;;                     (progn

;;                       (format t "~&rt! @ ~D,~D" x y)
;;                       (format t "~c" (aref board x (1+ y)) )
;;                       (format t " from is-line = ~A ~&" (is-line x (1+ y)  board))
;;                       (push (cons x (1+ y)) moves)
;;                       (setf (aref board  x (1+ y)) #\-)
;;                       (if (dfs board (1- (1- x)) y moves (1+ depth))
;;                           (return-from dfs t)
;;                           (progn
;;                             (format t "POP")
;;                             (format t "~Dx,~Dy" x y)
;;                             (pop moves)
;;                             ;; (print-board board)
;;                             (format t "   ~:C~&" (aref board x (1+ y)))
;;                             (setf (aref board x (1+ y)) #\Space)
;;                             (return-from dfs nil))))
;;                     nil)



;;                 ;;up
;;                 (if (and (allowable-line-x board (1- x)) (not (is-line (1- x) y board))
;;                          (not (format t "can go up? ~A" (not (is-line (1- x) y  board ))))
;;                          (not (format t "  @ board is ~C" (aref board (1- x) y)))
;;                          (not (format t " with c= ~A~&" (char= (aref board (1- x) y) #\|)))
;;                          (char= (aref board (1- x) y) #\|))
;;                     (progn

;;                       (format t "~&up! @ ~D,~D" x y)
;;                       (format t "~c" (aref board (1- x) y) )
;;                       (format t " from is-line = ~A ~&" (is-line (1- x) y  board))
;;                       (push (cons (1- x) y) moves)
;;                       (setf (aref board  (1- x) y) #\|)
;;                       (if (dfs board (1- (1- x)) y moves (1+ depth))
;;                           (return-from dfs t)
;;                           (progn
;;                             (format t "POP")
;;                             (format t "~Dx,~Dy" x y)
;;                             (pop moves)
;;                             ;; (print-board board)
;;                             (format t "   ~:C~&" (aref board (1- x) y))
;;                             (setf (aref board (1- x) y) #\Space)
;;                             (return-from dfs nil))))
;;                     nil)
;;                 )))))

(defun valid-node (board x y)
  (declare #.*common-optimization-settings*)
                                        ;(format t "validating node~%")
  (let ((count (connected-edge-count board x y)))
    (if (or (= count +goal+) (= count +goal-one+))
        t nil)))

(defun connected-edge-count (board x y)
  (declare #.*common-optimization-settings*
           (type fixnum x y))
  (loop for value in (list (is-line x (the fixnum (1+ y)) board)
                           (is-line x (the fixnum (1- y)) board)
                           (is-line (the fixnum (1+ x)) y board)
                           (is-line (the fixnum (1- x)) y board))
     with x fixnum = 0
     when value do (setf x (the fixnum (1+ x)))
     while (< x 3)
     finally (return x)))

;;; defmacro surounding-edges??
;; (aref board x (1+ y))
;; (aref board x (1- y))
;; (aref board (1+ x) y)
;; (aref board (1- x) y)

;; --- more notes... ---
;; Root behavior will be a chain of ifs...
;; if (can go up && !came from up)  {go up}
;; else if (can go right && ! came from right) {go right}
;; ...
;; at the end:
;; else {go back}
;; - Going backwards:
;; Since dfs only checks each direction once per node, one doesn't
;; necessarily have to remember each direction you came from.
;; When you reach a line (provided the node has only one line coming from it)
;; then you've exhausted all possibilities, so back up.
;; - Can go X :
;; Only checks the LINE COUNT of the affected squares.  If it does not EXCEED the
;; restraint, then it is a legal search-move.
;; If the space already is occupied, that is the direction you came from.
;; - Finding the solution:
;; If your current node has two lines coming from it, and it passed the "can go x" function,
;; run check-board.  it'll keep board-checking to a minimum.  if yes, you win.  If not,
;; IMMEDIATELY back up.

;; --- Placeholder ---
;; For when the AI wins.
(defun zomg-hax-win (board)
  (if board
      (format t "ZOMG HAX YOU WIN")))

;; ----STUPID idea----
;; What I want to do is rank the three (or less) available
;; moves by the probability that there will
;; be a line...  So cells with a 3 get a higher priority over
;; cells with just a 1 or 2 adjacent to them.
;; Cells with no number next to them get no boost.
;; Moves that intersect with a zero or if they GO OVER the cell/line count
;; automatically get pruned.  This "rank" will be calculated by
;; that edge's adjacent cells - there will be two for each possible
;; move.  So an edge with two [3]s next to it will get higher priority
;; than a [2] and a blank.
;; Then you can STEAL FIZZY LIFTING DRINKS - and BUMP into the CEILING
;; that has to be WASHED and STERILIZED so you GET NOTHING!
;; You LOSE!
;; GOOD DAY SIR!
;; ----/STUPID idea----
;; I said GOOD DAY!

;; we can add back tracking guides in later, such as if it was next to a 3 and four spaces would be covered from the expansion, go back.






;;HFS, don't look at anything below, start on your own please.




;; ;;; We only need to be able to tell whether it is *possible*
;; ;; that a line is at this location
;; ;; fortunately the sum of an odd and an even is always an odd,
;; ;; so a sum and check is much faster than
;; ;; three logical operations and four checks.
;; ;; Too clever for my own good? Probably.
;; ;; Cannonical version below:
;; ;;   (or (and (oddp x) (evenp y)) (and (evenp x) (oddp y)))
;; (defun is-it-a-line-position? (x y)
;;   (declare (optimize (speed 0) (safety 1))
;;            (type fixnum x y))
;;   (oddp (the fixnum (+ x y))))


;; ;;Maybe go based on the  + 's (vertexes) and pick an arbitrary direction



;; (defun check-node-solver (y x board)
;;   "Return true if there is an out-degree of 0 or 1. This means that the node is individually valid."
;;   (declare #.*common-optimization-settings*
;;            (type fixnum x y))
;;   (let ((x-limit (- (array-dimension board +X+) 1))
;;         (y-limit (- (array-dimension board +Y+) 1))
;;         (goal 0)
;;         (current 0))
;;     (declare (type fixnum x-limit y-limit goal current))
;;     ;; if the x value is NOT less than zero, do the math.
;;     (if (not (< (1- x) 0))
;;         (setf current (the fixnum (+ current (is-line y (1- x) board)))))
;;     (if (not (< (1- y) 0))
;;         (setf current (the fixnum (+ current (is-line (1- y) x board)))))
;;     (if (not (> (1+ x) (1- x-limit)))
;;         (setf current (the fixnum (+ current (is-line y (1+ x) board)))))
;;     (if (not (> (1+ y) (1- y-limit)))
;;         (setf current (the fixnum (+ current (is-line (1+ y) x board)))))
;;     (if (or (= current goal) (= current 1)) T nil)))

;; 1 check if solved
;; 2 check if there is more than 1 line leaving
;; 3 dfs in all directions not the line.
;;   this involves
;;   1 putting a line in the direction we are going
;;   2 pushing the move into the moves list
;;   Last; calling dfs with the coordinates offset by 2

;; (defun dfs (board x y moves) ; maybe add a depth so that we don't get too far in?
;;   (format t "~%~%~%")
;;   (print-board board)
;;   (format t "~A" moves)
;;   ;; current position must be >= 0, and < board dimension for x and y
;;   ;; if not return fail
;;   (if (check-board board)
;;       (return-from dfs t)
;;       (progn
;;         ;;; this is similar to something dana did for check-node (?)
;;         ;; first count the number of out vectors and their directions.
;;         (if (check-node-solver y x board)
;;             ;; second figure out which directions are available
;;             (let ((x-limit (- (array-dimension board +X+) 1))
;;                   (y-limit (- (array-dimension board +Y+) 1)))
;;               (format t "~&check-node-solver says : ~A~&" (check-node-solver y x board))
;;               (format t "going... ")
;;               (if (and (not (< (1- x) 0)) ;not out of bounds going up
;;                        (not (is-line-solver y (1- x) board))) ; there is not already a line there
;;                   (progn
;;                     (format t "up!~&")
;;                     (push (cons x y) moves)
;;                     (setf (aref board  (1- x) y) #\|)
;;                     (if (dfs board (1- (1- x)) y moves)
;;                         (return-from dfs t) ; could push move here instead
;;                         (progn (pop moves) (setf (aref board  (1- x) y) #\Space)))))
;;               (if (and (not (< (1- y) 0)) ; not oob left
;;                        (not (is-line-solver (1- y) x board)))
;;                   (progn
;;                     (format t "left!~&")
;;                     (push (cons x y) moves)
;;                     (setf (aref board x (1- y)) #\-)
;;                     (if (dfs board x (1- (1- y)) moves)
;;                         (return-from dfs t) ; could push move here instead
;;                         (progn (pop moves) (setf (aref board x (1- y)) #\Space)))))
;;               (if (and (not (> (1+ x) (1- x-limit))) ; not oob down
;;                        (not (is-line-solver y (1+ x) board)))
;;                   (progn
;;                     (format t "down!~&")
;;                     (push (cons x y) moves)
;;                     (setf (aref board  (1+ x) y) #\|)
;;                     (if (dfs board (1+ (1+ x)) y moves)
;;                         (return-from dfs t) ; could push move here instead
;;                         (progn (pop moves) (setf (aref board  (1+ x) y) #\Space)))))
;;               (if (and (not (> (1+ y) (1- y-limit))) ; not oob right
;;                        (not (is-line-solver (1+ y) x board)))
;;                   (progn
;;                     (format t "right!~&")
;;                     (push (cons x y) moves)
;;                     (setf (aref board x  (1+ y)) #\-)
;;                     (if (dfs board x (1+ (1+ y)) moves)
;;                         (return-from dfs t) ; could push move here instead
;;                         (progn (pop moves) (setf (aref board x  (1+ y)) #\Space))))))))))


;; possible number of solutions
;; (* (* 2 number-of-corners)
;;    (* 3 number of edge points)
;;    (* 4 number of internal points))

(defconstant +corners+ 4)
                                        ;(* (- (* 2 (+ (1- x-dim) (1- y-dim)) +corners+)
                                        ;x and y are the dimensions in boxes
(defun paths (n)
  (let ((x n)
        (y n))
    (* (* 2 +corners+)
       (* 3 (* 2 (+ (1- x) (1- y))))
       (* 4 (* (- x 1) (- y 1))))))
