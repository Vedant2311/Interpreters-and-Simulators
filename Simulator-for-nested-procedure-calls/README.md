# Simulator for nested procedure calls

In this project, the core ideas for understanding implementations of the static scoping discipline in Imperative languages, particularly the visibility rules and the set-up/tear-down involved in procedure call and return, have been implemented. The bodies of the main program and all the procedures are not specified.  Instead the simulator will take a command line argument and perform the necessary operation, as if you were providing the procedure body "on the fly".

At any point in the simulation, you should be able to see: 

1. the call stack (what procedures have been called so far, in the calling sequence
2. What procedures can be called from the current procedure frame at the top of the stack
3. All the named variables accessible from the current procedure frame, and their current values.
4. The static link chain or the display registers.

Instead of generating code for the procedures, an interactive environment -- a read-execute-display loop, has been developed. The users can inout the following commands:

1. Set the value of a variable to a given value/expression. (e.g.  x := y or x := 3 or...) If x or y is not accessible, an informative error message will be displayed. 

2. Call a procedure with specified arguments (e.g.,  P(c,7) or  S(a,5) or ...) -- If the procedure call is legal, then the called procedure's frame will be stacked up with the parameters bound to the actual arguments and the static links/display registers, and the dynamic links correctly updated.  If the call is not legal (procedure is not callable, or the arguments do not match the number of parameters), then an informative error message is displayed.

3. Return from the current procedure to caller. The current call stack will be popped, and the static links/display registers correctly updated.

4. A show command (show;) to just display the four contents described above. Acts as a null command

## About the project

1. A2.mll is the lexer
2. A3.mly is the parser
3. A6.ml is the code for performing the various functions mentioned above
4. test_a5.ml is the tester code which contains the Read-execute-display loop

## Running the code

Build all these codes and then run the REPL in the Ocaml terminal by using "#use test_a5.ml". A sample test case is given in the end of the test_a5.ml (Line: 116 onwards). Please go through these cases to get an overview of how to use this toolbox. 

In general, you can do the following four operations:

  1. **show;** -> Shows the four outputs mentioned.
  2. **P(4,5);** -> Will call P with the parameters as: x |-> 4 and y |-> 5 :- (Given the Input parameters for the Procedure P are x,y in the given order)
  3. **return;** => Will return from the call and give the updated registers accordingly
  4. **x:=4;** => Will set the Variable "x" to 4, if accesible!
