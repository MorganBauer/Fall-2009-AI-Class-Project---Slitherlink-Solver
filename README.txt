by Morgan Bauer and Dana Preble

Load by
(load "loader")

On a linux-computer with clisp, you may run by

    ./run

however you will not land in the debugger if there is an error; itwill just kick you back to the command line.

then do 
(in-package slither)
(slither)
OR
(slither:slither)
to run



Placing a move puts a line there
Placing it again removes it.


Typing 'quit' while playing the game will exit back to the menu.

Do (slither:run-tests) to test necessary functions.

can load from a file specified by the user. loads automatically a basic game.


Two included libraries were obtained
    lisp-unit for unit testing, from http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html
    split-sequence for parsing, from http://ftp.linux.org.uk/pub/lisp/experimental/cclan/split-sequence.tar.gz
