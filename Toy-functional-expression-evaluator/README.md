# Toy-functional-expression-evaluator

The programming language is created with the domain of the expressions given below:

    type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

    type  exptree =
      Var of string
      | N of int    
      | B of bool   
      | Abs of exptree                 
      | Negative of exptree            
      | Not of exptree
      | Add of exptree * exptree         
      | Sub of exptree * exptree         
      | Mult of exptree * exptree        
      | Div of exptree * exptree         
      | Rem of exptree * exptree         
      | Conjunction of exptree * exptree 
      | Disjunction of exptree * exptree 
      | Equals of exptree * exptree  
      | GreaterTE of exptree * exptree   
      | LessTE of exptree * exptree      
      | GreaterT of exptree * exptree    
      | LessT of exptree * exptree       
      | InParen of exptree             
      | IfThenElse of exptree * exptree * exptree 
      | Tuple of int * (exptree list)
      | Project of (int  * int) * exptree  
      | Let of definition * exptree
      | FunctionAbstraction of string * exptype * exptree
      | FunctionCall of exptree * exptree

    and definition =
        Simple of string * exptype* exptree
      | Sequence of (definition list)
      | Parallel of (definition list)
      | Local of definition * definition

The input is given by the user in the forms of strings which is converted into tokens by the tokenizer and then the sequence of tokens is given to the parser which gives meanings to these tokens and converts them into the expression types as given above. These expression types are checked for type errors and then these expression types are evaluated by an Eval function or a Stack Machine structure and this way, we obtain the output for any expression given by the user.

## About the files
The file A0.ml contains a big-int package, where integers are implemented as list of digits to carry out certain arithmetic operations for arbitary large numbers. A0.mli is the corresponding signature file

The file A1.ml has the definitions for all the data types (like the **expression tree**, **definition type** etc) and has the **eval** function for syntax and the **stack machine** functions for the semantics (regarding the evaluations of the input expressions). A1.mli is the corresponding signature file

The file A2.mll has the lexer, and A3.mly has the parser, making up the **front end**

The file A4.ml has the type checking functions defined. A4.mli is the corresponding signature file

The file test_a4.ml is a checker file for this program which works by combining all the modules made and then running the different kinds of test cases. The test cases described in it are for the type checking evaluations only. The testing functions for the evaluation and other functionalities can be made easily in the same manner

## How to run
To run the project, build all the a0.cmo, a1.cmo, a2.cmo, a3.cmo, a4.cmo and then open the Ocaml terminal and type: #use "test_a4.ml" 

Check the functions you can directly implement as well as the format of the input required for these different files from their Signature files. Though, the input formats given in the test_a4 file can make it very clear as to how to give the input programs or call functions etc 




