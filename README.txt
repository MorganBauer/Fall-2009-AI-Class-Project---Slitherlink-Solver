Morgan Bauer and Dana Preble

At the lisp prompt load by

    (load "loader")

then do 

    (in-package slither)
    (slither) 

To start the game.

Placing a move puts a line there
Placing it again removes it.

Typing 'quit' while playing the game will exit back to the menu.

Typing 'solve' while playing the game will solve the currently loaded board.

Do (run-tests) to test necessary functions.

It loads automatically the basic 2x2 game.

It can load from a file specified by the user.


To load a game, do

(slither)
y at prompt
n at prompt
type name of file at prompt


Two included libraries were obtained
    lisp-unit for unit testing, from     http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html
    split-sequence for parsing, from http://ftp.linux.org.uk/pub/lisp/experimental/cclan/split-sequence.tar.gz
