# Interpreters and Simulators
Consists of a Functional language as well as an Imperative language, implemented using Ocaml. Here, you can find three directories as listed below. The **Toy-functional-expression-evaluator** and **CBV-and-CBN-interpreted-language** correspond to the functional langauge implementation, while **Simulator-for-nested-procedure-calls** corresponds to the imperative language implementation. While more details of these modules are documented in a README in their corresponding directories, a brief writeup follows as below:

## Toy-functional-expression-evaluator
A toy programming language capable of evaluating expressions, type checking etc. with definite lexing and parsing

## CBV-and-CBN-interpreted-language
An Implementation of Call-by-Value (SECD machine) and Call-by-Name (Krivine Machine) semantics in Ocaml. The same front-end is used for this system as was implemented in **Toy-functional-expression-evaluator**.

## Simulator-for-nested-procedure-calls
An implementation of static scoping discipline in Imperative languages. The simulator will take a command line argument and perform the necessary operation, as if you were providing the procedure body "on the fly", thus creating a REPL loop. The same front-end is used for this system as was implemented in **Toy-functional-expression-evaluator**.
