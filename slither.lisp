(in-package #:slither)
;;(in-suite slither-tests)

(defconstant +X+ 0)
(defconstant +Y+ 1)

(defun slither ()
  "Game goes here"
  (format t "Welcome to slither")
  (if (y-or-n-p "Play slither?")
      (if (y-or-n-p "Load game automatically?(You can choose to load from a filename later)")
          (progn
            (format t "Loading game1.txt")
            (probe-file "game1.txt")
            (game-loop (multiple-value-call #'parse-board (read-board-from-file "game1.txt"))))
          (progn
            (format t "Please enter a file name (no quotes, e.g. game1.txt not \"game1.txt\")")
            (game-loop (multiple-value-call #'parse-board (read-board-from-file (loop-until-file-exists)))))
          )))


(defun game-loop (board)
  ;;(format t "Starting Game")
  (let ((moves nil))
    (loop
       (print-board board)
       (loop for move = (query-move)
          do (setf move (parse-move move board))
          until move
          finally (progn
                    (push move moves)
                    (place-move move board)))
       (if (check-board board)
           (progn
             (format t "You WIn!")
             (print-moves (nreverse moves))
             (return)
             )))))

(defun print-moves (moves)
  (format t "Your moves in order were:")
  (loop for move in moves
     :do (format t "~&   ~A~&" move)))


(defun query-move ()
  (format *query-io* "Where would you like to place a line?~&")
  (format *query-io* "Moves must be in a triplet with a space as a delimiter: ")
  (read-line *query-io*))

(defun parse-move (string board)
  (declare (optimize (speed 1) (safety 1) (space 1)
                     (compilation-speed 1) (debug 1))
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
  (case char
    (#\t t)
    (#\b t)
    (#\l t)
    (#\r t)))

(defun allowable-board-x (x board)
  (if (AND (> x 0)
           (<= x (/ (1- (array-dimension board +X+)) 2)))
      t
      nil))

(defun allowable-board-y (y board)
  (if (AND (> y 0)
           (<= y (/ (1- (array-dimension board +Y+)) 2)))
      t
      nil))

(defun place-move (triplet board)
  (let ((herex (1+ (* 2 (1- (car triplet)))))
        (herey (1+ (* 2 (1- (cadr triplet)))))
        (move (char-downcase (caddr triplet))))
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


;; 1. print board
;; 2. query player for a position
;;    Update board as result of position
;; 3. checks



;; I want to have cells with four edges, and the center number.
;; It would all be stored in an array,
;; the array for a 5x5 of numbers in the boxes needed, is (2*n + 1) for each side,
;; 1 for each of the numbers, and 6 for the points and edges between.

;;Maybe two arrays, one for the center, and one for the points


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

(defun constraint-checker ()
  (if (OR (not-continuous-loop) (violates-numbers-p))
      (keep-playing)
      (:GAME-WIN)))

;;;; Classes
;;;These class declarations are nowhere near complete and are just a basic structure to maybe hang ideas upon.

(defclass grid ()
  (faces edges vertices))

(defclass face ()
  (( order)
   ( edges )
   (vertices)))

(defclass edge ()
  (faces vertices))

(defclass vertex ()
  (faces edges))

;; Maybe a board class to hold an array? But what else can it hold?
;; The array contains a structured edition of the game board
;; so it can be printed out easily
(defclass board ()
  (array))


;;; This sucks, but it should work for initializing elements into an array
(defun basic-initialize-board (board)
  (loop :for i :from 0 :to (1- (array-dimension board 0))
     :do (loop :for j :from 0 :to (1- (array-dimension board 1))
            :do (cond ((and (evenp i) (evenp j)); fill in vertexes
                       (setf (aref board i j) #\+))
                      ((and (evenp i) (oddp j)); fill in top/bottom edges/lines
                       (setf (aref board i j) #\-))
                      ((and (oddp i) (evenp j)); fill in left/right edges/lines
                       (setf (aref board i j) #\|))
                      ((and (oddp i) (oddp j)) ; fill in faces
                       (setf (aref board i j) #\Space))))))

(defconstant +offset+ 1)

(defun print-board (board)
  (let ((board-dimension-x (array-dimension board 0))
        (board-dimension-y (array-dimension board 1)))
    (loop :for row :from 0 :to (+ board-dimension-x +offset+)
       :do (loop :for column :from 0 :to (+ board-dimension-y +offset+)
              :do (progn
                    (cond ((AND (zerop row) (evenp row))
                           (format t "~A" (/ column 2)))
                          ((AND (zerop column) (evenp column))
                           (format t "~A " (/ row 2))))))
       :do (format t "~&"))))

;;; Read in a board from the user
(defun read-board-from-user ()
  (let
      ((dimensions (read-board-dimensions)))
    (format t "Enter the lines with spaces in between the numbers~&")
    (format t "and using 'n' for a blank space.~&")
    (loop :for i :from 1 :to (cdr dimensions) ;; this is the y/height
       collecting (read-board-row i))))

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
    (let* ((board-size (split-sequence #\Space (read-line file-stream) :remove-empty-subseqs t)) ;we are reading the first line for the dimensions of the board.
           (height (parse-integer (cadr board-size))))
      (values (mapcar #'parse-integer board-size)
              (loop for i from 1 to height
                 do (format t "Reading ~:r line...~&" i)
                 collecting (read-line file-stream))))))

;; Tests for reading game form a file

(defun test-read-board-from-file ()
  (AND (equal (MULTIPLE-VALUE-LIST (read-board-from-file "game1.txt"))
              '((2 2)
                ("3 3"
                 "n n")))
       (equal (MULTIPLE-VALUE-LIST (read-board-from-file "game2.txt"))
              '((5 5)
                ("n n n 2 0"
                 "2 3 2 n 2"
                 "2 1 n 3 n"
                 "n 1 2 n 3"
                 "n 2 n n n")))))

;; Parse the board read in from file or user.
;; (multiple-value-list (read-board-from-file "game2.txt")

(defun parse-board (dimensions strings)
                                        ;(format t "~A" strings)
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
  (format t "~&")
  (loop :for row :from 0 :to (array-dimension board +X+)
     :do (loop :for column :from 0 :to (array-dimension board +Y+)
            :do (cond ((= row 0) ;if we are printing the first-line
                       (cond ((OR (= column 0) (oddp column))
                              (format t " "))
                             ((evenp column)
                              (format t "~D" (/ column 2)))))
                      ((= column 0)
                       (cond ((oddp row)
                              (format t " "))
                             ((evenp row)
                              (format t "~D" (/ row 2)))))
                      ((AND (> row 0) (> column 0))
                       (format t "~A" (aref board (1- row) (1- column))))))
     :do (format t "~&")))

;;;; Count out-edges
;;; use when traversing blindly (or not?)
;; if out edges if out edges are  two (not including the in edge) or more OR more than 3 (including the in edge), then fail and backtrack. It is not clear which case would be better to optimize for

;; (out-breadth vertex) => a number n from 0 to 4, or 1 to 3, or something

;; a function that coounts the crossings when moving ACROSS the board
;; must have an even number of crossings
;; application of even-odd rule


;;; Theoretically I want test-game funciton that we give it a name, and a test games function where we gice it a list of games.
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

;;; Turn the following into a test for the board.
;; (setf *answer-lines* (with-open-file (in "/home/morgan/Documents/programming/lisp/cl/slitherlink/game2solution.txt")
;;                                 (loop for line = (read-line in nil nil)
;;                                    while line
;;                                    collect line into lines
;;                                    finally (return lines))))
;; (defparameter *answered-board* (make-array '(11 11) :initial-contents (with-open-file (in "game2solution.txt") 
;;                                                                 (loop for line = (read-line in nil nil)
;;                                                                    while line
;;                                                                    collect line into lines
;;                                                                    finally (return lines)))))
;; (check-board *answered-board*)
(time (loop for i from 0 to 60000 ;takes approximately 1 second
         do (check-board *answered-board*)))

(defun check-board (view)
  (declare (type array view))
  (let ((x-limit (array-dimension view +X+))
        (y-limit (array-dimension view +Y+))
        (x 1)
        (y 1)
        (return-val T))
    (declare (type fixnum x y x-limit y-limit))
    ;; first, check the numbers
    (loop
       (loop
          (if (not (check-space y x view) )
              (setf return-val nil))
          (setf x (incf x 2))
          (when (> x (1- x-limit))
            (progn
              (setf x 1)
              (return))))
       (setf y (incf y 2))
       (when (> y (1- y-limit))
         (progn
           (setf y 1)
           (return)))
       ;; end of checking the spaces
       )
    ;; now check the pluses
    (setf x 0)
    (setf y 0)
    ;; almost copypasta from before
    (loop
       (loop
          (if (not (check-node y x view))
              (setf return-val nil))
          (setf x (+ x 2))
          (when (> x (1- x-limit))
            (progn
              (setf x 0)
              (return))))
       (setf y (+ y 2))
       (when (> y (1- y-limit))
         (progn
           (setf y 0)
           (return)))
       ;; end of checking the nodes
       )
    return-val))


(defun check-space (y x view)
  (declare (type fixnum x y))
  (let ((goal (aref view y x)))
    ;;(format t "goal = ~D~&" goal)
    (if (not (characterp goal))
        (progn
          (let ((current (+ (is-line (1+ y) x view)
                            (is-line (1- y) x view)
                            (is-line y (1+ x) view)
                            (is-line y (1- x) view))))
            (declare (type fixnum current))
            ;;(format t "goal = ~D current = ~D" goal current)
            (if (= current goal) T nil)))
        T)))

;; Here is a bit more tricky.  Gotta see where we are.
;; return true if current matches either goal or goal-two
;; Ignore the corresponding side if the number is negative or out-of-bounds
(defun check-node (y x view)
  (declare (type fixnum x y))
  (let ((x-limit (- (array-dimension view +X+) 1))
        (y-limit (- (array-dimension view +Y+) 1))
        (goal 0)
        (goal-two 2)
        (current 0))
    (declare (type fixnum x-limit y-limit goal goal-two current))
    ;; if the x value is NOT less than zero, do the math.
    (if (not (< (1- x) 0 ) )
        (setf current (+ current (is-line y (1- x) view))))
    (if (not (< (1- y) 0 ) )
        (setf current (+ current (is-line (1- y) x view))))
    (if (not (> (1+ x) x-limit ) )
        (setf current (+ current (is-line y (1+ x) view))))
    (if (not (> (1+ y) y-limit ) )
        (setf current (+ current (is-line (1+ y) x view))))
    (if (or (= current goal) (= current goal-two) ) T nil)))


;;;; is-line
;; Pass in the position in the array
;; If it is a line, return a 1
(defun is-line (y x view)
  (declare (optimize (speed 0) (safety 0) (space 0)
                     (compilation-speed 0) (debug 0))
           (type fixnum x y);(type simple-array view)
           )
  "Pass in the position in the array
If it is a line, return a 1"
  (let ((ret-val 0))
    (declare (type fixnum ret-val))
    (if (char= #\| (character (aref view y x)))
        (setf ret-val 1))
    (if (char= #\- (character (aref view y x)))
        (setf ret-val 1))
    (if (char= #\Space (character (aref view y x)))
        (setf ret-val 0))
    ret-val))

