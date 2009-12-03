;;Morgan Bauer & Dana Preble

(in-package #:slither)

(eval-when (:compile-toplevel)
  (defparameter *common-optimization-settings*
    '(optimize
      (speed 0)
      (safety 3)
      (space 0)
      (debug 0)
      (compilation-speed 0))
    "The common optimization settings used by declaration expressions."))

(defconstant +X+ 0)
(defconstant +Y+ 1)

(defun slither ()
  "Game goes here"
  (format t "Welcome to slither~&")
  (format t "The goal of the game is to make a single continuous loop
around the board without crossing or meeting up.
There is only one solution.
Moves are made in triplets like \"1 3 t\"
This signifies the box at 1 down and 3 over,
with the line being placed above the box.
If there is already a line there, it is removed.
if you wish to quit, type \"quit\" at the game prompt.~&")
  (loop while (y-or-n-p "Play slither?")
     do (if (y-or-n-p "Load game automatically?(You can choose to load from a filename later)")
            (progn
              (format t "Loading game1.txt")
              (probe-file "game1.txt")
              (game-loop (multiple-value-call #'parse-board
                           (read-board-from-file "game1.txt"))))
            (progn
              (format t "Please enter a file name")
              (format t "(no quotes, e.g. game1.txt not \"game1.txt\")")
              (game-loop (multiple-value-call #'parse-board
                           (read-board-from-file (loop-until-file-exists))))))))




;; 1. print board
;; 2. query player for a position
;;    Update board as result of position
;; 3. checks

(defun game-loop (board)
  ;;(format t "Starting Game~&")
  (let ((moves nil)
        (starting-time (get-universal-time)))
    (loop
       (print-board board) ; 1.Print Board
       (loop for move = (query-move) ; 2. Query player
          ;;do (format t "~A" move)
          do (when (string-equal "quit" (string-trim " " move))
               (format t "Quitting current game")
               (return-from game-loop))
          do (when (string-equal "solve" (string-trim " " move))
               (format t "Solving")
               (time (solve board))
               (return-from game-loop))
          do (setf move (parse-move move board))
          until move
          finally (progn
                    ;;(format t "~A" move)
                    (push move moves)
                    (place-move move board)))
       (if (check-board board)
           (progn
             (format t "You Win!~&")
             (print-moves (nreverse moves))
             (format t " In ~D seconds" (- (get-universal-time) starting-time))
             (return))))))

(defun print-moves (moves)
  (format t "Your moves in order were:")
  (loop for move in moves
     :do (format t "~&   ~A~&" move)))


(defun query-move ()
  (format *query-io* "Where would you like to place a line?~&")
  (format *query-io* "Moves must be in a triplet with a space as a delimiter: ")
  (read-line *query-io*))

(defun parse-move (string board)
  (declare #.*common-optimization-settings*
           (type string string))
  (let ((possible-move (split-sequence #\Space string :remove-empty-subseqs t)))
    (declare (type list possible-move))
    (if (= 3 (length possible-move))
        (if (AND (if (numberp (parse-integer (car possible-move) :junk-allowed t))
                     (allowable-board-x (parse-integer (car possible-move)) board)
                     nil)
                 (if (numberp (parse-integer (cadr possible-move) :junk-allowed t))
                     (allowable-board-y (parse-integer (cadr possible-move)) board)
                     nil)
                 (if (= 1 (length (caddr possible-move)))
                     (valid-move-p (character (caddr possible-move)))
                     nil))
            (progn ;(format t "we have a vaild move,maybe")
              (list (parse-integer (car possible-move))
                    (parse-integer (cadr possible-move))
                    (character (caddr possible-move))))
            nil)
        nil)))


(defun valid-move-p (char)
  (declare #.*common-optimization-settings*
           (type character char))
  (case char
    (#\t t)
    (#\b t)
    (#\l t)
    (#\r t)))

(defun allowable-board-x (x board)
  (declare #.*common-optimization-settings*
           (type fixnum x))
  (if (AND (> x 0)
           (<= x (/ (1- (array-dimension board +X+)) 2)))
      t
      nil))

(defun allowable-board-y (y board)
  (declare #.*common-optimization-settings*
           (type fixnum y))
  (if (AND (> y 0)
           (<= y (/ (1- (array-dimension board +Y+)) 2)))
      t
      nil))

(defun place-move (triplet board)
  (let ((herex (1+ (* 2 (1- (car triplet)))))
        (herey (1+ (* 2 (1- (cadr triplet)))))
        (move (char-downcase (caddr triplet))))
    ;; Probably possible to use a case statement here (for speed),
    ;; but the computer doesn't mind if it has to wait
    ;; (i.e. user-input is much slower than the computer).
    (cond ((char= #\t move)
           (if (char= (aref board (1- herex) herey) #\Space)
               (setf (aref board (1- herex) herey) #\-)
               (setf (aref board (1- herex) herey) #\Space)))
          ((char= #\b move)
           (if (char= (aref board (1+ herex) herey) #\Space)
               (setf (aref board (1+ herex) herey) #\-)
               (setf (aref board (1+ herex) herey) #\Space)))
          ((char= #\l move)
           (if (char= (aref board herex (1- herey)) #\Space)
               (setf (aref board herex (1- herey)) #\|)
               (setf (aref board herex (1- herey)) #\Space)))
          ((char= #\r move)
           (if (char= (aref board herex (1+ herey)) #\Space)
               (setf (aref board herex (1+ herey)) #\|)
               (setf (aref board herex (1+ herey)) #\Space))))))

;; I want to have cells with four edges, and the center number.
;; It would all be stored in an array,
;; the array for a 5x5 of numbers in the boxes needed, is (2*n + 1) for each side, or 11x11
;; 1 for each of the numbers, and 6 for the points and edges between.

;;;; Mathematical Observations
;;; This whole thing is very graph theory.

;; the whole 'game' is a grid. The grid has faces, edges, and points.
;; Each face, has a number of points and edges that are equal,
;; in this case, four.

;; Each edge has two points and two faces to connect to.
;; If one of the faces is NIL, than this edge is an edge edge. (on the outsde, the outermost.)

;; Each point connects to four edges, and four faces.
;; If some of the faces or edges are NIL, than this is an outside point.
;; It can either be on a point or an edge, certain things can be known here,
;; such as that the if it is a 3, it has to connect so that the corner isn't where a terminator is, or rather, that the point on the inside is connected.

;; The grid needs, for squares
;; n*m faces,
;; n+1*m+1 points, and
;; ((n+1*m) + (m+1*n)) edges
;; ALSO
;; the 'Euler Characteristic' which is (faces + points - 2) except in this case -1 because we don't count the 'outside' as a face.


;; The loop that is made, has an 'inside' and an 'outside' so, if you got stuck, you could solve the game by counting the number of crosses you make while traversing from one side of the grid to the other. The number of crossings must be even.

;;; This reminds me of some of the rendering techniques and such used to determine whether we are inside or outside of a polygon.

;;;; Constraints of solving the game
;; Must be one single continuous loop (out-degree of each vertex is 2)
;; Must not violate the numbers

(defun read-board-row (current-line-number)
  (format t "~&Please insert ~:r line~&" current-line-number))

(defun read-board-dimensions ()
  (format t "What are the dimensions of the board?~&")
  (format t "Enter as NxM, where N and M are numbers.~&")
  (let
      ((dimensions (mapcar #'parse-integer
                           (split-sequence #\x (read-line)
                                           :remove-empty-subseqs t))))
    (cons (car dimensions) (cadr dimensions))))



;;; Loop
(defun loop-until-file-exists ()
  (loop for filename = (read-line *query-io*)
     until (probe-file filename)
     do (format t "Incorrect file name")
     finally (return filename)))

;;; Read in a board from a file
(defun read-board-from-file (filename)
  (with-open-file (file-stream filename :direction :input)
    (let* ((board-size (split-sequence #\Space (read-line file-stream) :remove-empty-subseqs t)) ; We are reading the first line for the dimensions of the board.
           (height (parse-integer (cadr board-size))))
      (values (mapcar #'parse-integer board-size)
              (loop for i from 1 to height
                 ;;do (format t "Reading ~:r line...~&" i)
                 collecting (read-line file-stream))))))

;; Tests for reading game from a file

(define-test read-board-from-file
  (assert-equal (MULTIPLE-VALUE-LIST (read-board-from-file "game1.txt"))
                '((2 2)
                  ("3 3"
                   "n n")))
  (assert-equal (MULTIPLE-VALUE-LIST (read-board-from-file "game2.txt"))
                '((5 5)
                  ("n n n 2 0"
                   "2 3 2 n 2"
                   "2 1 n n 3"
                   "n 1 2 n 3"
                   "n 2 n n n"))))

;; Parse the board read in from file or user.
;; (multiple-value-list (read-board-from-file "game2.txt")

(defun parse-board (dimensions strings)
  ;;(format t "~A" strings)
  (let ((board (make-properly-sized-array dimensions)))
    ;;(print (array-dimensions board))
    (loop :for row fixnum :from 0 :to (1- (array-dimension board +X+))
       ;;:do (format t "row #~d~&" row)
       :do (loop :for column :from 0 :to (1- (array-dimension board +Y+))
              ;; :do (format t "row# ~D column #~d~&" row column)
              ;; :do (format t "~A~&" board )
              :do (cond ((and (evenp row) (evenp column)) ;+
                         (setf (aref board row column) #\+))
                        ((and (oddp row) (oddp column)) ;fill faces
                         (setf (aref board row column)
                               ;;if a number, fill a number otherwise, it is not a number so use a space
                               (if (parse-integer (nth (/ (1- column) 2)(split-sequence #\Space (nth (/ (1- row) 2) strings))) :junk-allowed t)
                                   (parse-integer (nth (/ (1- column) 2)(split-sequence #\Space (nth (/ (1- row) 2) strings))) :junk-allowed t)
                                   #\Space))))))
    board))

(defun make-properly-sized-array (dimensions)
  (make-array (list (proper-size (cadr dimensions)) (proper-size (car dimensions))) :initial-element #\Space))

(defun proper-size (dimension)
                                        ;(format t "proper-dimension is ~D~&" (1+ (* 2 dimension)))
  (1+ (* 2 dimension)))

(defun print-board (board)
  (format t "~&") ; Make sure to start on a fresh line.
  (loop :for row :from 0 :to (array-dimension board +X+)
     :do (loop :for column :from 0 :to (array-dimension board +Y+)
            :do (cond
                  ((= row 0) ; If we are printing the first-line
                   (cond ((= 0 column)
                          (format t "     "))
                         ((oddp column) ;non numbered columns
                          (format t " "))
                         ((evenp column) ; numbered columns
                          (format t "~3D  " (/ column 2)))))
                  ((= column 0) ; we are printing the first row, which contains the row numbers
                   (cond ((oddp row)
                          (format t "     "))
                         ((evenp row)
                          (format t "~3D  " (/ row 2)))))
                  ((AND (> row 0) (> column 0)) ;the actual board
                   (format t "~3A" (aref board (1- row) (1- column))))))
     :do (format t "~2&")))





;;;; Count out-edges
;;; use when traversing blindly (or not?)
;; if out edges if out edges are  two (not including the in edge) or more OR more than 3 (including the in edge), then fail and backtrack. It is not clear which case would be better to optimize for

;; (out-breadth vertex) => a number n from 0 to 4, or 1 to 3, or something

;; a function that coounts the crossings when moving ACROSS the board
;; must have an even number of crossings
;; application of even-odd rule

;;; Theoretically I want test-game funciton that we give it a name, and a test games function where we give it a list of games (i.e. a list of strings that are filenames to load games from).
;; (test game1
;;       "Checks if game1 can be solved"
;;       (is (= (solve "game1.txt") (parse-solution "game1solution.txt"))))

;; (test game2
;;       "Checks if game2 can be solved"
;;       (is (= (solve "game2.txt") (parse-solution "game2solution.txt"))))

;; Check the numbers
;; numbers are on the odd-odd pairs
;; Then check all of the +'s
;; If the index of the add/sub is either negative or beyond the column or row length,
;; disregard


;; (time (loop for i from 0 to 175000 ;takes approximately 1 second
;;          do (check-board *answered-board*)))



;(declaim (inline is-line check-space check-node))


(defun check-board (view)
  (declare   #.*common-optimization-settings*
             (type array view))
  (let ((x-limit (array-dimension view +X+))
        (y-limit (array-dimension view +Y+))
        (x 1)
        (y 1)
        (return-val T))
    (declare (type fixnum x y x-limit y-limit))
    ;; Make two functions here, and do something like
                                        ; (if (and (check-spaces ...) (check-nodes ...))
                                        ;     (you-win)
                                        ;     (loop))
    ;; first, check the numbers constraints
    (loop
       (loop
          (if (not (check-space y x view) )
              (setf return-val nil))
          (incf x 2)
          (when (> x (1- x-limit))
            (progn
              (setf x 1)
              (return))))
       (incf y 2)
       (when (> y (1- y-limit))
         (progn
           (setf y 1)
           (return))))
    ;; end of checking the spaces
    ;; now check the pluses
    (setf x 0)
    (setf y 0)
    ;;Check nodes
    (loop
       (loop
          (if (not (check-node y x view x-limit y-limit))
              (setf return-val nil))
          (incf x 2)
          (when (> x (1- x-limit))
            (progn
              (setf x 0)
              (return))))
       (incf y 2)
       (when (> y (1- y-limit))
         (progn
           (setf y 0)
           (return)))) ;; end of checking the nodes
    return-val))

(defun check-space (y x view)
  (declare #.*common-optimization-settings*
           (type fixnum x y)
           (inline is-line))
  (let ((goal (aref view y x)))
    ;;(format t "goal = ~D~&" goal)
    (if (not (characterp goal))
        (progn
          (let ((current (the fixnum (+ (is-line (1+ y) x view)
                                        (is-line (1- y) x view)
                                        (is-line y (1+ x) view)
                                        (is-line y (1- x) view)))))
            (declare (type fixnum current))
            ;;(format t "goal = ~D current = ~D" goal current)
            (if (= current goal) T nil)))
        T)))

;; Here is a bit more tricky.  Gotta see where we are.
;; return true if current matches either goal or goal-two
;; Ignore the corresponding side if the number is negative or out-of-bounds
(defun check-node (y x board x-limit y-limit)
  "Return true if there is an out-degree of 0 or 2. This means that the node is individually valid."
  (declare #.*common-optimization-settings*
           (type fixnum x y))
  (let (;(x-limit (- (array-dimension board +X+) 1))
        ;(y-limit (- (array-dimension board +Y+) 1))
        (goal 0)
        (goal-two 2)
        (current 0))
    (declare (type fixnum x-limit y-limit goal goal-two current))
    ;; if the x value is NOT less than zero, do the math.
    (if (not (< (1- x) 0))
        (setf current (the fixnum (+ current (is-line y (1- x) board)))))
    (if (not (< (1- y) 0))
        (setf current (the fixnum (+ current (is-line (1- y) x board)))))
    (if (not (> (1+ x) (1- x-limit)))
        (setf current (the fixnum (+ current (is-line y (1+ x) board)))))
    (if (not (> (1+ y) (1- y-limit)))
        (setf current (the fixnum (+ current (is-line (1+ y) x board)))))
    (if (or (= current goal) (= current goal-two) ) T nil)))

;;;; is-line
;; Pass in the position in the array
;; If it is a line, return a 1
(defun is-line (y x view)
  (declare #.*common-optimization-settings*
           (type fixnum x y))
  "Pass in the position in the array
If it is a line, return a 1"
  (let ((char (aref view y x)))
    (declare (type character char))
    (case char
      (#\| 1)
      (#\- 1)
      (#\Space 0))))

(define-test checker
  (let ((game1solution (make-array '(5 5)
                                   :initial-contents (with-open-file (in "game1solution.txt")
                                                       (loop for line = (read-line in nil nil)
                                                          while line
                                                          collect line into lines
                                                          finally (return lines)))))
        (game2solution (make-array '(11 11)
                                   :initial-contents (with-open-file (in "game2solution.txt")
                                                       (loop for line = (read-line in nil nil)
                                                          while line
                                                          collect line into lines
                                                          finally (return lines))))))
    (assert-true (check-board game1solution))
    (assert-true (check-board game2solution))))



;;;;Arbitrary benchmark
;; (defparameter *game2solution* (make-array '(11 11)
;;                                    :initial-contents (with-open-file (in "game2solution.txt")
;;                                                        (loop for line = (read-line in nil nil)
;;                                                           while line
;;                                                           collect line into lines
;;                                                           finally (return lines)))))
;; (time (loop for x from 1 to 150000
;;                do (check-board game2solution)))