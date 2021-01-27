# CBV-and-CBN-interpreted-tiny-functional-language

A tiny functional language is implemented here, of the form: e ::= x | \x:t.e | e_1 (e_2)

Here, **e** is the expression, **x** is a variable, and **t** is the supposed type for the variable **x**

Certain imperative operations are also defined like e1 + e2 etc. Here, a Krivine machine and a SECD machine have been implemented for the Call-by-Name and Call-by-Value semantics respectively.

## About the files

The file A0.ml contains a big-int package, where integers are implemented as list of digits to carry out certain arithmetic operations for arbitary large numbers. A0.mli is the corresponding signature file

The file A2.mll has the lexer, and A3.mly has the parser => Making up the front end

The file A1.ml contains the execution of the program. Here, both the CBV and CBN semantics have been implemented. A type checker is also implemented in this file to give out an error if an ill typed expression is given as input 

The file test_a5.ml is the tester file and it contains the execute functions, bindings etc. for the program to run. There are also a few sample programs in that code. Please go through them to get an idea about the working of the program and how to use it

## How to run

To run the project, build all the a0.cmo, a1.cmo, a2.cmo, a3.cmo and then open the Ocaml terminal and type: #use "test_a5.ml" 

After that, call any function and operate it on any expression as desired.

The Functions for the Krivine and SECD machines and the general inputs to them are given as below:

1. krivine g p binding stk stk1 => 

        (i) g: The tuple-list where the type for a corresponding variable is there (Eg. [("Y",Tint)])
        (ii) p: The input to be evaluated
        (iii) binding: The tuple-list for the value-closure corresponding to the variables in the input expression are there (Eg. [("Y",ValueClosure(NumVal(5),[]))])
        (iv) stk: The stack of the unexecuted variables. To be passed as [] ONLY
        (v) stk1: The "type" stack, storing the most recent unimplemented Imperative operation on it's top (Like "Mult", "Add" etc). To be passed as [] ONLY
  
2. secd stk binding p d =>

        (i) stk: The unexecuted variables stack. To be passed as [] ONLY
        (ii) binding: The tuple-list for the answer type (Semantics) corresponding to the variables in the input expression are there (Eg. [("Y", Num(mk_big 5))])
        (iii) p: The program to be executed
        (iv) d: The Dump. To be passed as [] ONLY

 
