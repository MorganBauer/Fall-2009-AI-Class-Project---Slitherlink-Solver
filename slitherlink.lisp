(in-package #:slitherlink)
(in-suite slitherlink-tests)

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
;;   (faces edges))

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
                       (setf (aref board i j) #\*))
                      ((and (evenp i) (oddp j)); fill in top/bottom edges
                       (setf (aref board i j) #\-))
                      ((and (oddp i) (evenp j)); fill in left/right lines
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
                           (format t "~A" (/ row 2))))))
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
                           (split-sequence #\x (read-line) :remove-empty-subseqs t))))
    (cons (car dimensions) (cadr dimensions))))

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

(test read-board-from-file
  (is (equal (MULTIPLE-VALUE-LIST (read-board-from-file "game1.txt"))
             '((2 2)
               ("3 3"
                "0 0")))
      (equal (MULTIPLE-VALUE-LIST (read-board-from-file "game2.txt"))
             '((5 5)
               ("n n n 2 0"
                "2 3 2 n 2"
                "2 1 n 3 n"
                "n 1 2 n 3"
                "n 2 n n n")))))


;; Parse the board read in from file or user.


(defun parse-board (rows columns))


  ;; Count out edges
;;; use when traversing blindly (or not?)
  ;; if out edges if out edges are  two (not including the in edge) or more OR more than 3 (including the in edge), then fail and backtrack. It is not clear which case would be better to optimize for

  ;; (out-breadth vertex) => a number n from 0 to 4, or 1 to 3, or something

  ;; a function that coounts the crossings when moving ACROSS the board
  ;; must have an even number of crossings
  ;; application of even-odd rule


;;; Theoretically I want test-game funciton that we give it a name, and a test games function where we gice it a list of games.
  (test game1
    "Checks if game1 can be solved"
    (is (= (solve "game1.txt") (parse-solution "game1solution.txt"))))

  (test game2
    "Checks if game2 can be solved"
    (is (= (solve "game2.txt") (parse-solution "game2solution.txt"))))

