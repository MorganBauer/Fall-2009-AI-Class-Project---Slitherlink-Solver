(in-package #:slither)

(defparameter *visited* (make-array (list 0 0)))

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


;;(declaim (inline is-it-a-line-position?))



(defun solve (board)
  (let ((starting-time (get-universal-time)) ; remember starting time
        (ending-time nil)
        (moves nil)) ; create empty list for moves
    (setf *visited* (make-array (array-dimensions board) :initial-element nil))
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
  ;; The compiler complains that this code cannot be reached. which is true. Heh. --mhb
  ;;(format t "~&holy moly, there is no solution!~%This should not be possible!~&")
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
        (start-pt-y nil)
        (starting-time (get-universal-time ))
        (ending-time 0))
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
    ;; (values start-pt-x start-pt-y)
    ;; So unless the board was completely blank, start-pt-x and start-pt-y should have a
    ;; square with a number in it.  It favors 3s over 2s, and 2s over 1s.  It overlooks spaces
    ;; and zeroes.
    ;; DFS from the upper-left corner of the number...
    (if (dfs board (- start-pt-x 1) (- start-pt-y 1)  moves)
        ;;(zomg-hax-win board)
        (progn
          (setf ending-time (get-universal-time))
          (format t "we have found the solution")
          (print-board board)
          (print-moves (nreverse moves))
          (format t "~&Solve took ~D seconds" (- ending-time
                                                 starting-time)))
        ;; else (ie you don't win) DFS from the bottom-right corner...
        (if (dfs board (+ start-pt-x 1) (+ start-pt-y 1)  moves)
            ;;            (zomg-hax-win board)
            (progn
              (setf ending-time (get-universal-time))
              (format t "we have found the solution")
              (print-board board)
              (print-moves (nreverse moves))
              (format t "~&Solve took ~D seconds" (- ending-time
                                                     starting-time)))

            ()))))

;; Are we having fun yet?
;; I wish to do this recursively. It may make the code a little easier, but perhaps
;; make the process slower...
(defun valid-line-p (board x y)
  (declare #.*common-optimization-settings*
           (type fixnum x y)
           (type array board))
  (when (valid-board-position-p board x y) ; not outside the board
    (is-line board x y)))

;; mhb thinks that checking on either side of the newly placed line that the faces are correct, will help us back out faster
;;; dfs is a depth=first-search
;; it assumes that any lines placed are correct, and any x's placed are correct.
(defun dfs (board x y moves
            &optional (depth 0) (visited (make-array (array-dimensions board) :initial-element nil)))
  (declare #.*common-optimization-settings*
           (type fixnum x y)
           (type array board)
           (type list moves))
  (if (> depth (1- (* (array-dimension board +X+) (array-dimension board +Y+))))
      (progn
        ;; (format t "depth exceeded, too deep, returning")
        (return-from dfs nil))
      (progn
        ;; (format t "~2%")
        (if (check-board board)
            ;; (progn (format t "~10%HOORAY~10%")
            (return-from dfs board))
        (progn
          ;; (print-board board)
          (let ((edge-count (edge-count board x y)))
            (if (< edge-count 3) ; less than 3 edges leaving from x,y
                (progn
                  (flet ((;;;; Local modified version of is-line
                          ;; Pass in the position in the array
                          ;; If it is a line, return true
                          emptyp (board x y)
                           (declare #.*common-optimization-settings*
                                    (type fixnum x y))
                           "Pass in the position in the array
If it is a line, return a 1"
                           ;; (format t "~&is-line!!!~&")
                           (let ((char (aref board x y)))
                             (declare (type character char))
                             (case char
                               (#\| nil)
                               (#\- nil)
                               (#\Space t)
                               (#\x t)))))
                    ;; (format t "node-valid~%")

                    ;;If there is already a line, we should follow it, otherwise dfs at point
                    ;; do this by counting edges at point, if 2, then wee need to follow one.

                    (if (= 2 edge-count) ; follow other lines code to go here,
                        (progn ;do stuff to follow a line
                          ;;Find which edge is present, but unvisited, and visit and follow it.
                          (let ((x (1+ x))
                                (y y))
                            (if (and (valid-board-x-p board x)
                                     (not (emptyp board x y)))
                                ;; If location has not been visited
                                ;; and we can continue.
                                (if (and (not (aref visited x y))
                                         (can-continue-p board x y))
                                    (progn
                                      (setf (aref visited x y) t) ; 'Visit' the line
                                      (push (cons x y) moves) ; save move.
                                      (if (dfs board (1+ x) y moves (1+ depth) visited)
                                          (return-from dfs t)
                                          (progn ; else this is not on the solution path
                                            (pop moves)
                                            (setf (aref visited x y) nil)))))))

                          ;; right (+y)
                          (let ((x x)
                                (y (1+ y)))
                            (if (and (valid-board-y-p board y)
                                     (not (emptyp board x y)))

                                        ; If location has not been visited
                                (if (and (not (aref visited x y))
                                         (can-continue-p board x y))
                                    (progn
                                      (setf (aref visited x y) t) ; 'Visit' the line
                                      (push (cons x y) moves) ; save move.
                                      (if (dfs board x (1+ y) moves (1+ depth) visited)
                                          (return-from dfs t)
                                          (progn ; else this is not on the solution path
                                            (pop moves)
                                            (setf (aref visited x y) nil)))))))
                          ;; up (-x)
                          (let ((x (1- x))
                                (y y))
                            (if (and (valid-board-x-p board x)
                                     (not (emptyp board x y)))
                                        ; If location has not been visited
                                (if (and (not (aref visited x y))
                                         (can-continue-p board x y))
                                    (progn
                                      (setf (aref visited x y) t) ; 'Visit' the line
                                      (push (cons x y) moves) ; save move.
                                      (if (dfs board (1- x) y moves (1+ depth) visited)
                                          (return-from dfs t)
                                          (progn ; else this is not on the solution path
                                            (pop moves)
                                            (setf (aref visited x y) nil)))))))
                          ;; left (-y)
                          (let ((x x)
                                (y (1- y)))
                            (if (and (valid-board-y-p board y)
                                     (not (emptyp board x y)))
                                        ; If location has not been visited
                                (if (and (not (aref visited x y))
                                         (can-continue-p board x y))
                                    (progn
                                      (setf (aref visited x y) t) ; 'Visit' the line
                                      (push (cons x y) moves) ; save move.
                                      (if (dfs board x (1- y) moves (1+ depth) visited)
                                          (return-from dfs t)
                                          (progn ; else this is not on the solution path
                                            (pop moves)
                                            (setf (aref visited x y) nil)))))))
                          )

                        (progn ;else, dfs over there

         ;;;; Begin recursing
                          ;; These four things should be a macro.
                          ;; down (+x)
                          (let ((x (1+ x))
                                (y y))
                            ;; (format t "check down")
                            ;; (format t "    X=~2d,Y=~2D~&" x y)
                            (if (and (valid-board-x-p board x)
                                     (emptyp board x y))
                                (progn
                                  ;; (format t " X=~d,Y=~D & going down ~&" x y)
                                  ;; (format t "     char = ~A ~&" (aref board x y))
                                  (setf (aref board x y) #\|
                                        (aref visited x y) t)
                                  ;; check either side
                                  (if (can-continue-p board x y)
                                      (progn
                                        (push (cons x y) moves)
                                        (if (dfs board (1+ x) y moves (1+ depth) visited)
                                            (return-from dfs t)
                                            (progn
                                              (pop moves)
                                              (setf (aref board x y) #\Space
                                                    (aref visited x y) nil))))
                                      (setf (aref board x y) #\Space
                                            (aref visited x y) nil)))))

                          ;; right (+y)
                          (let ((x x)
                                (y (1+ y)))
                            ;; (format t "check right")
                            ;; (format t "      X=~2d,Y=~2D~&" x y)
                            (if (and (valid-board-y-p board y)
                                     (emptyp board x y))
                                (progn
                                  ;; (format t " X=~d,Y=~D & going right ~&" x y)
                                  ;; (format t "     char = ~A ~&" (aref board x y))
                                  (setf (aref board x y) #\-
                                        (aref visited x y) t)
                                  (if (can-continue-p board x y)
                                      (progn
                                        (push (cons x y) moves)
                                        (if (dfs board x (1+ y) moves (1+ depth) visited)
                                            (return-from dfs t)
                                            (progn
                                              (pop moves)
                                              (setf (aref board x y) #\Space
                                                    (aref visited x y) nil))))
                                      (setf (aref board x y) #\Space
                                            (aref visited x y) nil)))))

                          ;; up (-x)
                          (let ((x (1- x))
                                (y y))
                            ;; (format t "check up")
                            ;; (format t "    X=~2d,Y=~2D~&" x y)
                            (if (and (valid-board-x-p board x)
                                     (emptyp board x y))
                                (progn
                                  ;; (format t " X=~d,Y=~D & going up ~&" x y)
                                  ;; (format t "     char = ~A ~&" (aref board x y))
                                  (setf (aref board x y) #\|
                                        (aref visited x y) t)
                                  (if (can-continue-p board x y)
                                      (progn
                                        (push (cons x y) moves)
                                        (if (dfs board (1- x) y moves (1+ depth) visited)
                                            (return-from dfs t)
                                            (progn
                                              (pop moves)
                                              (setf (aref board x y) #\Space
                                                    (aref visited x y) nil))))
                                      (setf (aref board x y) #\Space
                                            (aref visited x y) nil)))))

                          ;; left (-y)
                          (let ((x x)
                                (y (1- y)))
                            ;; (format t "check left")
                            ;; (format t "      X=~2d,Y=~2D~&" x y)
                            (if (and (valid-board-y-p board y)
                                     (emptyp board x y))
                                (progn
                                  ;; (format t " X=~d,Y=~D & going left ~&" x y)
                                  ;; (format t "     char = ~A ~&" (aref board x y))
                                  (setf (aref board x y) #\-
                                        (aref visited x y) t)
                                  (if (can-continue-p board x y)
                                      (progn
                                        (push (cons x y) moves)
                                        (if (dfs board x (1- y) moves (1+ depth) visited)
                                            (return-from dfs t)
                                            (progn
                                              (pop moves)
                                              (setf (aref board x y) #\Space
                                                    (aref visited x y) nil))))
                                      (setf (aref board x y) #\Space
                                            (aref visited x y) nil)))))
                          ;; (format t "ran out of moves to make, returning...")
                          nil))))))))))

(defun can-continue-p (board x y)
  (let ((char (aref board x y)))
    (declare (type character char))
    (case char
      (#\| ;; Vertical, so check left and right.
       (progn
         (AND
          (let ((y (1- y))) ;left
            (if (valid-board-y-p board y)
                (if (numberp (aref board x y))
                    (>= (aref board x y) (edge-count board x y))
                    t)
                t))
          (let ((y (1+ y))) ;right
            (if (valid-board-y-p board y)
                (if (numberp (aref board x y))
                    (>= (aref board x y) (edge-count board x y))
                    t)
                t)))))
      (#\- ;; Horizontal, co check above and below/
       (progn
         (AND
          (let ((x (1- x))) ;above
            (if (valid-board-x-p board x)
                (if (numberp (aref board x y))
                    (>= (aref board x y) (edge-count board x y))
                    t)
                t))
          (let ((x (1+ x))) ;below
            (if (valid-board-x-p board x)
                (if (numberp (aref board x y))
                    (>= (aref board x y) (edge-count board x y))
                    t)
                t))))))))

(define-test can-continue-p
  (let ((board (make-array '(3 3)
                           :initial-contents (list "   "
                                                   "-+ "
                                                   "2| "))))
    (setf (aref board 2 0) 2)
    (assert-true (can-continue-p board 2 1)))
  (let ((board (make-array '(3 3)
                           :initial-contents (list "   "
                                                   "-+ "
                                                   "1| "))))
    (setf (aref board 2 0) 1)
    (assert-false (can-continue-p board 2 1)))
  (let ((board (make-array '(3 3)
                           :initial-contents (list "+ +"
                                                   "|  "
                                                   "+ +"))))
    (assert-true (can-continue-p board 1 0)))
  (let ((board (make-array '(3 3)
                           :initial-contents (list "+ +"
                                                   "|  "
                                                   "+-+"))))
    (assert-true (can-continue-p board 2 1)))
  )


;; (defun profile-dfs ()
;;   (sb-profile:unprofile)
;;   (sb-profile:reset)
;;   (sb-profile:profile valid-board-x-p valid-board-y-p check-board is-line
;;                       edge-count can-continue-p apply-x-marks)
;;   (let ((board (read-board "game2.txt"))
;;         (moves nil))
;;     (dfs (apply-x-marks board) 0 0 moves 0))
;;   (sb-profile:report)
;;   (sb-profile:unprofile))




(defun valid-board-x-p (board x)
  (declare #.*common-optimization-settings*
           (type fixnum x) (type array board))
  (if (AND (>= x 0)
           (<= x (1- (array-dimension board +X+))))
      t nil))

(defun valid-board-y-p (board y)
  (declare #.*common-optimization-settings*
           (type fixnum y) (type array board))
  (if (AND (>= y 0)
           (<= y (1- (array-dimension board +Y+))))
      t nil))

(defun valid-board-position-p (board x y)
  (declare #.*common-optimization-settings*
           (type fixnum x) (type array board))
  (if (and (valid-board-x-p board x)
           (valid-board-y-p board y))
      t))

;; (defun valid-node (board x y)
;;   (declare #.*common-optimization-settings*
;;            (type array board)
;;            (type fixnum x y))
;;   ;; (format t "validating node~%")
;;   (let ((count (edge-count board x y)))
;;     (declare (type fixnum count))
;;     (when (or (= count +goal+) (= count +goal-one+))
;;       t)))

;; (defconstant +up+ (list 'x (list '1- 'y)))

(defun edge-count (board x y)
  (declare #.*common-optimization-settings*
           (type fixnum x y)
           (type array board))
  ;; I deem this an okay use of is-line, as I do not need the actual
  ;; direction, just a number. --mhb
  ;; (let (
  ;; (count (the fixnum 0))
  ;; (declare (type fixnum count))
  ;; )
  (declare (type list lines))
  ;; (if (if (valid-board-position-p board x (the fixnum (1+ y)))
  ;;         (is-line x (the fixnum (1+ y)) board))
  ;;     (incf count))
  ;; (if (if (valid-board-position-p board x (the fixnum (1- y)))
  ;;     (is-line x (the fixnum (1- y)) board))
  ;;     (incf count))
  ;; (if (if (valid-board-position-p board  (the fixnum (1+ x)) y)
  ;;     (is-line (the fixnum (1+ x)) y board))
  ;;     (incf count))
  ;; (if (if (valid-board-position-p board (the fixnum (1- x)) y)
  ;;     (is-line (the fixnum (1- x)) y board))
  ;;     (incf count))
  ;; (the fixnum count))
  ;; (format t "~A~%" lines)
  (loop for value in (list (if (valid-board-position-p board x (the fixnum (1+ y)))
                               (is-line board x (the fixnum (1+ y)) ))
                           (if (valid-board-position-p board x (the fixnum (1- y)))
                               (is-line board x (the fixnum (1- y)) ))
                           (if (valid-board-position-p board  (the fixnum (1+ x)) y)
                               (is-line board (the fixnum (1+ x)) y ))
                           (if (valid-board-position-p board (the fixnum (1- x)) y)
                               (is-line board (the fixnum (1- x)) y )))
     with x fixnum = 0
     when value do (setf x (the fixnum (1+ x)))
     ;; The while might be wanted if we only need to know it is greater
     ;; than 2 (or some other number) ,rather than the avtual number of edges
     ;; while (< x 3)
     finally (progn
               ;; (format t "  ~A lines connected~%" x)
               (return (the fixnum x))))
  )
;;)

;; nutty attempted macro
;; this was never going to work cause I was basing it on something broken
;; (macrolet ((crazy (bd direction lns)
;;                         `(let ((list ,direction)
;;                 (bd1 ,bd)
;;                 (ln1 ,lns))
;;             `(if (valid-board-position-p ,bd1 ,@list)
;;                  (push (is-line ,@list ,bd1) ,ln1)))))

(define-test dfs
  ;;;; Sanity-check to ensure that the checker actually checks.
  (let ((solution (read-board-solution "game1solution.txt")))
    (assert-true (check-board solution))
    ;; Solution with full hints
    (setf solution (read-board-solution "game2solutionFilled.txt"))
    (assert-true (check-board solution))
    ;; solution with no hints
    (setf solution (read-board-solution "game2solution.txt"))
    (assert-true (check-board solution)))

  ;;;; Now try solving
  (let ((board (read-board "game1.txt"))
        (moves nil))
    ;; Tiny board
    (assert-false (check-board board))
    (assert-true (dfs board 0 0 moves))
    (assert-true (check-board board))
    (let ((board (read-board "game2filled.txt"))
          (moves nil))
      ;; Filled board
      (assert-false (check-board board))
      (assert-true (dfs board 0 0 moves))
      (assert-true (check-board board))

      ;; Normal board
      )))


(define-test edge-count
  (flet ((quarter-turn (a)
           (let ((d2 (array-dimensions a)))
             (let ((d (car d2))
                   (b (make-array d2)))
               (let ((c (/ (- d 1) 2)))
                 (do ((i 0 (+ i 1))) ((= i d))
                   (do ((j 0 (+ j 1))) ((= j d))
                     (setf (aref b (+ (* -1 (- j c)) c) i ) (aref a i j)))))
               b))))

    (let ((4edges (make-array '(3 3)
                              :initial-contents (list " | "
                                                      "-+-"
                                                      " | ")))
          (3edges (make-array '(3 3)
                              :initial-contents (list " | "
                                                      "-+ "
                                                      " | ")))
          (2edges (make-array '(3 3)
                              :initial-contents (list " | "
                                                      "-+ "
                                                      "   ")))
          (1edges (make-array '(3 3)
                              :initial-contents (list "   "
                                                      "-+ "
                                                      "   "))))
      (assert-equal 4 (edge-count 4edges 1 1))
      (assert-equal 3 (edge-count 3edges 1 1))
      (assert-equal 2 (edge-count 2edges 1 1))

      (assert-equal 1 (edge-count 1edges 1 1))
      (setf 1edges (quarter-turn 1edges)) ; one-turn
      (assert-equal 1 (edge-count 1edges 1 1))
      (setf 1edges (quarter-turn 1edges)) ; two-turn
      (assert-equal 1 (edge-count 1edges 1 1))
      (setf 1edges (quarter-turn 1edges)) ; three-turns
      (assert-equal 1 (edge-count 1edges 1 1))
      (setf 1edges (quarter-turn 1edges)) ; four-turns, this should match the original.
      (assert-equal 1 (edge-count 1edges 1 1))
      )))

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
