open A0
exception Not_implemented

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* abstract syntax *)
type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of definition * exptree
  | FunctionAbstraction of string * exptype * exptree
  | FunctionCall of exptree * exptree
(* definition *)
and definition =
    Simple of string * exptype * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition


(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int | LET | FABS | FCALL
  | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF


(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)


exception Myexp of string
 
(*Some basic list functions*) 

let rec appendList l1 l2 = match l1 with 
                  [] -> l2
              |   x::xs -> x :: (appendList xs l2)
  



let rec iMember i y = match  i with
| 1 -> (
    
      match y with 
      x :: xs -> x
    |  [] -> raise (Myexp "Illformed Exp-Tree") 

    )

| x -> (

    match y with 
    y :: ys -> iMember (x-1) ys
  |  _ -> raise (Myexp "Illformed Exp-Tree") 


)

let rec actualFirst n l = match n with 

   0 -> []
  | n -> (

      match l with 

       x ::xs -> x :: (actualFirst (n-1) xs)
   |  _ -> raise (Myexp "Illformed stack")


  )

  let rec revList l = match l with
      [] -> []
    |   x::xs -> (revList xs) @ [x]


let firstTotal n stk = actualFirst n (revList stk)

  let rec length l= match l with
     [] -> 0
  |  x::xs -> 1 + length(xs)
  
(*The post order traversal, returning an opcode list*)
let rec compile ex = match ex with

    Var(s) -> [VAR(s)]
  | N(i) ->[NCONST(mk_big i)]
  | B(true) -> [BCONST(true)]
  | B(false) -> [BCONST(false)]
  | Abs(ex1) -> (compile ex1) @ [ABS]
  | Negative(ex1) -> (compile ex1) @ [UNARYMINUS]
  | Not(ex1) -> (compile ex1)@ [NOT]
  | Add(ex1, ex2) -> (appendList (compile ex1) (compile ex2)) @ [PLUS]
  | Sub(ex1, ex2) -> (appendList (compile ex1) (compile ex2)) @ [MINUS]
  | Mult(ex1, ex2) -> (appendList (compile ex1) (compile ex2)) @ [MULT]
  | Div(ex1, ex2) -> (appendList (compile ex1) (compile ex2)) @ [DIV]
  | Rem(ex1, ex2) -> (appendList (compile ex1) (compile ex2)) @ [REM]
  | Conjunction(ex1,ex2) -> (appendList (compile ex1) (compile ex2)) @ [CONJ]
  | Disjunction(ex1,ex2) -> (appendList (compile ex1) (compile ex2)) @ [DISJ]
  | Equals(ex1,ex2) -> (appendList (compile ex1) (compile ex2)) @ [EQS]
  | GreaterTE(ex1,ex2) -> (appendList (compile ex1) (compile ex2)) @ [GTE]
  | LessTE(ex1,ex2) -> (appendList (compile ex1) (compile ex2)) @ [LTE]
  | GreaterT(ex1,ex2) -> (appendList (compile ex1) (compile ex2)) @ [GT]
  | LessT(ex1,ex2) -> (appendList (compile ex1) (compile ex2)) @ [LT]
  | InParen(ex1) -> (compile ex1)@ [PAREN]
  | IfThenElse(ex1,ex2,ex3) -> ( appendList (appendList (compile ex1) (compile ex2)) (compile ex3)) @ [IFTE]
  | Tuple(n,y) -> 
let rec compileList l =( match  l with
 [] -> []
| x :: xs -> (compile x) @ (compileList xs )
) in ((compileList y) @ [TUPLE(n)])

  | Project((i,n),e) -> (compile e)@ [PROJ(i,n)]




(*The stack machine evaluator. Operates on the opcodes obtained from the compile function. Operates on the defined type of Big-int*)
let rec stackmc stk binding pgm = match pgm with 

   [] -> (

    match stk with
      x :: xs -> x
    |  _ ->  raise (Myexp "Illformed stack")

   )

 | VAR(s) :: ys ->  stackmc ((binding s)::stk) binding ys
 | NCONST(i) :: ys -> stackmc (Num(i) :: stk) binding ys
 | BCONST(true) :: ys -> stackmc (Bool(true) :: stk) binding ys
 | BCONST(false) :: ys -> stackmc (Bool(false) :: stk) binding ys

 | ABS :: ys -> (

  match stk with

      Num(x1) :: xs -> stackmc (Num(abs x1) :: xs) binding ys
    |  _ -> raise (Myexp "Illformed stack")


 )

 | UNARYMINUS :: ys -> (

  match stk with

      Num(x1) :: xs -> stackmc (Num(minus x1) :: xs) binding ys
    |  _ -> raise (Myexp "Illformed stack")


 )

 | NOT :: ys -> (

  match stk with

      Bool(x1) :: xs -> stackmc (Bool(not x1) :: xs) binding ys
    |  _ -> raise (Myexp "Illformed stack")


 )


 | PLUS :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Num(add x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | MINUS :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Num(sub x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | MULT :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Num(mult x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )


 | DIV :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Num(div x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )


 | REM :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Num(rem x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )
 
 | CONJ :: ys ->(

      match stk with 
              Bool(x1) :: (Bool(x2) :: xs) -> stackmc (Bool(x2 && x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | DISJ :: ys ->(

      match stk with 
              Bool(x1) :: (Bool(x2) :: xs) -> stackmc (Bool(x2 || x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | EQS :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Bool(eq x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | GT :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Bool(gt x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | LT :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Bool(lt x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | GTE :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Bool(geq x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | LTE :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> stackmc (Bool(leq x2 x1) :: xs) binding ys
            |   _ -> raise (Myexp "Illformed stack")


 )

 | PAREN :: ys ->  stackmc stk binding ys
 | IFTE :: ys -> (

     match stk  with
     | x1 :: x2 :: Bool(b) :: xs  -> (if b=true then (stackmc (x2 :: xs) binding ys) else (stackmc (x1 :: xs) binding ys))
     | _ -> raise (Myexp "Illformed stack")

 )

 | TUPLE(n) :: ys -> (

   if ((length stk) < n) then raise (Myexp "Illformed stack") else (let tempList = actualFirst n stk in stackmc (Tup(n,(revList tempList)) :: (revList (firstTotal ((length stk) -n) stk))) binding ys)

 )


 | PROJ(i,n) :: ys -> (

  if((i<=n) && (i>=1)) then (    
      match stk with 

      Tup(n, y) :: xs ->  stackmc ((iMember i y) :: xs) binding ys
   | _ -> raise (Myexp "Illformed stack")

    )


   else  raise (Myexp "Illformed stack")

 )


(*An evaluate function, which operates on the basic types of int, bool etc. No use of opcodes*)
let rec eval ex rho = match ex with
    Var(s) -> rho s
  | N(i) -> NumVal(i)
  | B(true) -> BoolVal(true)
  | B(false) -> BoolVal(false)
  | Abs(ex1) -> let NumVal(y)= (eval ex1 rho) in (if y>0 then NumVal(y) else NumVal(-y))
  | Negative(ex1) -> let NumVal(y)= (eval ex1 rho) in NumVal(- y)
  | Not(ex1) -> let BoolVal(b) = (eval ex1 rho) in BoolVal(not b)
  | Add(ex1, ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in NumVal(y1 + y2)
  | Sub(ex1, ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in NumVal( y1 - y2)
  | Mult(ex1, ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in NumVal(y1 * y2)
  | Div(ex1, ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in NumVal( y1/ y2)
  | Rem(ex1, ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in NumVal( y1 mod y2)
  | Conjunction(ex1,ex2) -> let BoolVal(b1) = (eval ex1 rho) and BoolVal(b2) = (eval ex2 rho) in BoolVal(b1 && b2)
  | Disjunction(ex1,ex2) -> let BoolVal(b1) = (eval ex1 rho) and BoolVal(b2) = (eval ex2 rho) in BoolVal(b1 || b2)
  | Equals(ex1,ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in BoolVal(eq (mk_big y1) (mk_big y2))
  | GreaterTE(ex1,ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in BoolVal(geq (mk_big y1) ( mk_big y2))
  | LessTE(ex1,ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in BoolVal(leq (mk_big y1)  (mk_big y2))
  | GreaterT(ex1,ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in BoolVal(gt (mk_big y1) (mk_big y2))
  | LessT(ex1,ex2) -> let NumVal(y1) = (eval ex1 rho) and NumVal(y2) = (eval ex2 rho) in BoolVal(lt (mk_big y1) (mk_big y2))
  | InParen(ex1) -> eval ex1 rho
  | IfThenElse(ex1,ex2,ex3) -> let BoolVal(b) = (eval ex1 rho) in (if b=true then (eval ex2 rho) else (eval ex3 rho))


  | Tuple(n,y) ->  let rec evalList l rho =( match l with 
     [] -> []
  |  x :: xs -> (eval x rho) :: (evalList xs rho)

)
in ( if((length y) =n) then TupVal(n, (evalList y rho)) else raise (Myexp "Illformed Exp-Tree1") )
  | Project((i,n),e) -> (

      match e with
        Tuple(p, y) -> if ((length y) = n) && (p=n)  then (if((i<=n)&&(i>=1)) then (eval (iMember i y) rho) else raise (Myexp "Illformed Exp-Tree") ) else raise (Myexp "Illformed Exp-Tree") 
      |   _ -> raise (Myexp "Illformed Exp-Tree2") 
  )

  









