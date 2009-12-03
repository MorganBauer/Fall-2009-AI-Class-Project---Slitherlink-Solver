(in-package #:slither)

;up  x-
;left y-
;right y+
;down x+

;;;; ACHTUNG!!!
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
    ;; Perhaps instead of picking an arbitrary starting position, start at one of the squares that
    ;; actually contains a number other than zero (therefore one of those four spaces has to have a line)
    ;; That way we avoid marking a place where there may not need a line at all.

    (find-a-plausible-starting-location-then-dfsolve) ; see below
    
    (loop for x fixnum from 0 to (array-dimension board +X+) by 2
       :do (loop for y fixnum from 0 to (array-dimension board +Y+) by 2
              ;; make a function pass it in.
              :do (if (is-vertex? x y) ; is this a dot, check ;;don't need this, I think
                      (progn
                        (assert (evenp x))
                        (assert (evenp y))
                        :do (format t "~&dfs at x:~A,y:~A~&" x y)
                        (if (dfs board x y moves) ; if it returns true, we have found a solutio
                            (progn
                              (setf ending-time (get-universal-time))
                              (format t "we have found the solution")
                              (print-board board)
                              (print-moves (nreverse moves))
                              (format t "~&Solve took ~D seconds" (- ending-time
                                                                     starting-time))
                              t))))))
    (format t "~&holy moly, there is no solution!~%This should not be possible!~&")))


(defun find-a-plausible-starting-location-then-dfsolve (board)
  ; A good starting location will be a cross that has a higher number on it.
  ; if the number is a three, then EVERY + will guarantee a solution. 
  ; at the most, if there are no 3's, then a 2 or 1 will suffice.  
  ; Then, worst case scenario, two dfs's will have to be done, 
  ; starting from opposite corners.
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
   (if (typep start-pt-x 'integer) (return) () )
   ) () ) ; elses tend to do nothing...

   ; debug line: outputs the x and y of the starting box
   (values start-pt-x start-pt-y)
   ; So unless the board was completely blank, start-pt-x and start-pt-y should have a 
   ; square with a number in it.  It favors 3s over 2s, and 2s over 1s.  It overlooks spaces
   ; and zeroes.  
   ; DFS from the upper-left corner of the number...
   (if (dfs (- start-pt-x 1) (- start-pt-y 1) board) (zomg-hax-win board) 
       ; else (ie you don't win) DFS from the bottom-right corner...
       (if (dfs (+ start-pt-x 1) (+ start-pt-y 1) board) (zomg-hax-win board) () )
   )))

  ; Are we having fun yet?
  ; I wish to do this recursively. It may make the code a little easier, but perhaps
  ; make the process slower...
  ; 
  (defun dfs (x y board ) ; maybe add a depth so that we don't get too far in?
    ())

  ; --- more notes... ---
  ; Root behavior will be a chain of ifs...
  ; if (can go up && !came from up)  {go up}
  ; else if (can go right && ! came from right) {go right}
  ; ...
  ; at the end:
  ; else {go back}
  ; - Going backwards: 
  ; Since dfs only checks each direction once per node, one doesn't
  ; necessarily have to remember each direction you came from.
  ; When you reach a line (provided the node has only one line coming from it) 
  ; then you've exhausted all possibilities, so back up.
  ; - Can go X :
  ; Only checks the LINE COUNT of the affected squares.  If it does not EXCEED the 
  ; restraint, then it is a legal search-move.
  ; If the space already is occupied, that is the direction you came from.
  ; - Finding the solution: 
  ; If your current node has two lines coming from it, and it passed the "can go x" function,
  ; run check-board.  it'll keep board-checking to a minimum.  if yes, you win.  If not, 
  ; IMMEDIATELY back up.

  ;; --- Placeholder ---
  ; For when the AI wins.
  (defun zomg-hax-win (board)
      (format t "ZOMG HAX YOU WIN"))

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





;HFS, don't look at anything below, start on your own please.
















;;; We only need to be able to tell whether it is *possible*
;; that a line is at this location
;; fortunately the sum of an odd and an even is always an odd,
;; so a sum and check is much faster than
;; three logical operations and four checks.
;; Too clever for my own good? Probably.
;; Cannonical version below:
;;   (or (and (oddp x) (evenp y)) (and (evenp x) (oddp y)))
(defun is-it-a-line-position? (x y)
  (declare (optimize (speed 0) (safety 1))
           (type fixnum x y))
  (oddp (the fixnum (+ x y))))


;;Maybe go based on the  + 's (vertexes) and pick an arbitrary direction



(defun check-node-solver (y x board)
  "Return true if there is an out-degree of 0 or 1. This means that the node is individually valid."
  (declare #.*common-optimization-settings*
           (type fixnum x y))
  (let ((x-limit (- (array-dimension board +X+) 1))
        (y-limit (- (array-dimension board +Y+) 1))
        (goal 0)
        (current 0))
    (declare (type fixnum x-limit y-limit goal current))
    ;; if the x value is NOT less than zero, do the math.
    (if (not (< (1- x) 0))
        (setf current (the fixnum (+ current (is-line y (1- x) board)))))
    (if (not (< (1- y) 0))
        (setf current (the fixnum (+ current (is-line (1- y) x board)))))
    (if (not (> (1+ x) (1- x-limit)))
        (setf current (the fixnum (+ current (is-line y (1+ x) board)))))
    (if (not (> (1+ y) (1- y-limit)))
        (setf current (the fixnum (+ current (is-line (1+ y) x board)))))
    (if (or (= current goal) (= current 1)) T nil)))

(defun is-line-solver (y x view)
  (declare #.*common-optimization-settings*
           (type fixnum x y))
  "Pass in the position in the array
If it is a line, return a 1"
  (let ((char (aref view x y)))
    (declare (type character char))
    (format t "~& char at ~A,~A is ~:C~&" x y char)
    (case char
      (#\| t)
      (#\- t)
      (#\Space nil))))



;; 1 check if solved
;; 2 check if there is more than 1 line leaving
;; 3 dfs in all directions not the line.
;;   this involves
;;   1 putting a line in the direction we are going
;;   2 pushing the move into the moves list
;;   Last; calling dfs with the coordinates offset by 2



(defun dfs (board x y moves) ; maybe add a depth so that we don't get too far in?
  (format t "~%~%~%")
  (print-board board)
  (format t "~A" moves)
  ;; current position must be >= 0, and < board dimension for x and y
  ;; if not return fail
  (if (check-board board)
      (return-from dfs t)
      (progn
        ;;; this is similar to something dana did for check-node (?)
        ;; first count the number of out vectors and their directions.
        (if (check-node-solver y x board)
            ;; second figure out which directions are available
            (let ((x-limit (- (array-dimension board +X+) 1))
                  (y-limit (- (array-dimension board +Y+) 1)))
              (format t "~&check-node-solver says : ~A~&" (check-node-solver y x board))
              (format t "going... ")
              (if (and (not (< (1- x) 0)) ;not out of bounds going up
                       (not (is-line-solver y (1- x) board))) ; there is not already a line there
                  (progn
                    (format t "up!~&")
                    (push (cons x y) moves)
                    (setf (aref board  (1- x) y) #\|)
                    (if (dfs board (1- (1- x)) y moves)
                        (return-from dfs t) ; could push move here instead
                        (progn (pop moves) (setf (aref board  (1- x) y) #\Space)))))
              (if (and (not (< (1- y) 0)) ; not oob left
                       (not (is-line-solver (1- y) x board)))
                  (progn
                    (format t "left!~&")
                    (push (cons x y) moves)
                    (setf (aref board x (1- y)) #\-)
                    (if (dfs board x (1- (1- y)) moves)
                        (return-from dfs t) ; could push move here instead
                        (progn (pop moves) (setf (aref board x (1- y)) #\Space)))))
              (if (and (not (> (1+ x) (1- x-limit))) ; not oob down
                       (not (is-line-solver y (1+ x) board)))
                  (progn
                    (format t "down!~&")
                    (push (cons x y) moves)
                    (setf (aref board  (1+ x) y) #\|)
                    (if (dfs board (1+ (1+ x)) y moves)
                        (return-from dfs t) ; could push move here instead
                        (progn (pop moves) (setf (aref board  (1+ x) y) #\Space)))))
              (if (and (not (> (1+ y) (1- y-limit))) ; not oob right
                       (not (is-line-solver (1+ y) x board)))
                  (progn
                    (format t "right!~&")
                    (push (cons x y) moves)
                    (setf (aref board x  (1+ y)) #\-)
                    (if (dfs board x (1+ (1+ y)) moves)
                        (return-from dfs t) ; could push move here instead
                        (progn (pop moves) (setf (aref board x  (1+ y)) #\Space))))))))))


#|
at current position,

four progns in a progn

(progn
  (progn
    (push up)
    (if (dfs new-position)
        (return t)
        (pop up)
        ))
  (progn
    (push left)
    ...)
  ...)
|#

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
