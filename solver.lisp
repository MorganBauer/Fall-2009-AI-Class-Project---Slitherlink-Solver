(in-package #:slither)



;;;; ACHTUNG!!!
;; Upon furthur review, I believe it would be simplest to dfs based on the points.
;; this is not what I have below, but something similar follows from what I have below. Do an 'is-point?' function instead to check where points are, then simply dfs from the direction that is not already occupied. if there are more than two edges leading away from the current point, return failure, so that we backtrack.

;;also, almost certainly we can use a macro at some point for shortening stuff up, but probably don't need too.


                                        ;(declaim (inline is-it-a-line-position?))

(defun solve (board)
  (let ((starting-time (get-universal-time)) ; remember starting time
        (moves nil)) ; create empty list for moves
    ;; when dfs'ing on dots, we can go from 0 to (1- dimension-limit) by 2
    (loop for x from 0 to (array-dimension board +X+) by 2
       :do (loop for y from 0 to (array-dimension board +Y+) by 2
              ;; make a function pass it in.
              :do (if (is-it-a-dot? x y) ; is this a dot, check ;;don't need this, I think
                      (progn
                        (assert (evenp x))
                        (assert (evenp y))
                        :do (format t "~&dfs at x:~A,y:~A~&" x y)
                        (if (dfs board x y moves) ; if it returns true, we have found a solutio
                            (progn
                              (format t "we have found the solution")
                              (print-board board)
                              (print-moves (nreverse moves))
                              (format t "~&Solve took ~D seconds" (- (get-universal-time)
                                                                   starting-time))
                              t))))))
    (format t "~&holy moly, there is no solution!~%This should not be possible!~&")))

(defun is-it-a-dot? (x y)
  (declare (type fixnum x y))
  (= 0 x y)) ;this is just for testing purposes, it should solve the 2x2 board 
;  (and (evenp x) (evenp y)))


;; 1 check if solved
;; 2 check if there is more than 1 line leaving
;; 3 dfs in all directions not the line.
;;   this involves
;;   1 putting a line in the direction we are going
;;   2 pushing the move into the moves list
;;   Last; calling dfs with the coordinates offset by 2

(defun dfs (board x y moves) ; maybe add a depth so that we don't get too far in?
  ())

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
