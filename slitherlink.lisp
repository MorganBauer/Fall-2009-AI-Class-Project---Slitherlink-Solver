(in-package #:slitherlink)
(in-suite slitherlink-tests)

;; I want to have cells with four edges, and the center number.
;; It would all be stored in an array,
;; the array for a 5x5 of numbers in the boxes needed, is (2*n + 1) for each side,
;; 1 for each of the numbers, and 6 for the points and edges between.

;;Maybe two arrays, one for the center, and one for the points


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

;;; This sucks, but it should work for initializing elements into an array
(defun basic-initialize-array (array)
  ((loop :for i :from 0 :to 8
      :do (loop :for j :from 0 :to 8
             :do (cond ((and (evenp i) (evenp j)); fill in vertexes
                        (setf (aref array i j) #\*))
                       ((and (evenp i) (oddp j)); fill in top/bottom edges
                        (setf (aref array i j) #\|))
                       ((and (oddp i) (evenp j)); fill in left/right lines
                        (setf (aref array i j) #\|))
                       ((and (oddp i) (oddp j)) ; fill in faces
                        (setf (aref array i j) #\0)))))))

(defun read-game ()
  (loop :for i = 0 :then (1+ i)
     do (progn
          (format t "Please insert ~:r line" (1+ i))
          (split-sequence #\Space (read-line) :remove-empty-subseqs t))))



(defconstant +offset+ 1)

(defun print-board (board-dimension)
  (loop :for row :from 0 :to (* board-dimension 2)
     :do (loop :for column :from 0 :to (* board-dimension 2)
            :do (progn
                 (cond ((AND (zerop row) (evenp row))
                      (format t "~A " column))
                     ((AND (zerop column) (evenp column))
                      (format t "~A " row)))))
     :do (format t "~%~%")))

