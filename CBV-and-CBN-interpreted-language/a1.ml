open A0
exception Not_implemented

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Tfunc of (exptype * exptype) 

type  exptree =
 V of string 
 | Lambda of (exptree * exptype * exptree) 
 | App of (exptree * exptree) 
 | Plus of (exptree * exptree) 
 | Mult of (exptree * exptree) 
 | And of (exptree * exptree) 
 | Or of (exptree * exptree) 
 | Bool of bool 
 | Integer of int 
 | Cmp of exptree 
 | If_Then_Else of (exptree * exptree * exptree)


(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | RET | PLUS | MULT | CONJ | DISJ | CMP | COND of opcode list * opcode list | APP | CLOS of string * opcode list 

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool 

(* Answer type*)
type answer = Num of bigint | Bool of bool | ValClosure of string * opcode list * (string * answer) list 

type closure = Clos of exptree * (string * closure) list | ValueClosure of  value * (string * closure) list

exception Myexp of string


(* Type checking functions *)

let rec presentIn s g = match g with 

  [] -> false
|  x :: xs -> (

  match x with 

   (a,b) -> (if a=s then true else (presentIn s xs))

   | _ -> presentIn s xs

)

let rec findIn s g = match g with 

  [] -> Tunit
|  x :: xs -> (

  match x with 

   (a,b) -> (if a=s then b else (findIn s xs))

   | _ -> findIn s xs

)

let rec presentIn1 s g = match g with 

  [] -> false
|  x :: xs -> (

  match x with 

   (a,b) -> (if a=s then true else (presentIn1 s xs))

   | _ -> presentIn1 s xs

)

let rec findIn1 s g = match g with 


  x :: xs -> (

  match x with 

   (a,b) -> (if a=s then b else (findIn1 s xs))

   | _ -> findIn1 s xs

)


let appendTable g x t1 = let yY = (x,t1) in yY::g

let appendTable1 g x t1 = let yY = (x,t1) in yY::g


let rec appendTables p g = match p with 

  [] -> g
  | x::xs -> x :: (appendTables xs g)

let rec removeThis str g = match g with 

  [] -> []
  | (x,t) :: xs -> (if (x=str) then (removeThis str xs) else ( (x,t) :: (removeThis str xs)) ) 



let rec allEq l = match l with 

  [] -> true
  | x -> true
  | x :: y :: xs -> if (x=y) then (allEq (y::xs)) else false

let rec findeq l = match l with

  [] -> Tunit
  | x::xs -> if (x=Tunit) then (findeq xs) else x

let rec mklist s p = if p=0 then [] else (s :: (mklist s (p-1)))

let rec findIth i l = match l with 

  x :: xs -> if i=1 then x else (findIth (i-1) l)


(*Returns the list of the possible types of the variable x appearing in the expression e*)
let rec findListPos s x e g= match e with 

   V (str) ->  if str=x then (

    match s with 

    [] -> []   (* Means that f(x) = x*)
    | p:: xs -> if (p="Add") then [Tint]
            else(

              
                if p="And" then [Tbool]
                  else(

                        []

                       ) 

                  )
              )
        
      else []



  | Integer(i) -> []
  | Bool(b) ->  []
  (* binary operators on integers *)
  | Plus(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  | Mult(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  (* binary operators on booleans *)
  | And(e1,e2) -> (findListPos ("And" :: s) x e1 g) @ (findListPos ("And" :: s) x e2 g)
  | Or(e1,e2) -> (findListPos ("And" :: s) x e1 g ) @ (findListPos ("And" :: s) x e2 g)
  (* comparison operations on integers *)
  | Cmp(e1) -> (findListPos ("Add" :: s) x e1 g) 
  (* a conditional expression *)
  | If_Then_Else(e0,e1,e2) -> (findListPos ("And" :: s) x e0 g) @ (findListPos s x e1 g ) @ (findListPos s x e2 g)


  | Lambda(e3,t,e1) -> (match e3 with 

                          V(y)->  (

                                   if(x=y) then (

                                          [t] @ (findListPos s x e1 g)

                                          ) else (findListPos s x e1 g)


                                  )

                         | _ -> [Tunit]
                      )

  | App(e1,e2) -> (

        match e1 with 

          Lambda(e4,t,e3) -> (match e4 with 


                               V(y) ->      if (x=y) then [t] @(findListPos s x e3 g) @ (findListPos s x e2 g) else (findListPos s x e3 g) @ (findListPos s x e2 g)
                              | _ -> [Tunit]



                            )


    
          | _ -> [Tunit]

  )

  


(*Obtains the type of expression e w.r.t table g*)
let rec obtainType g e = match e with

   V(s) ->  if (presentIn s g) then (findIn s g) else Tunit
  | Integer(i) -> Tint
  | Bool(b) ->  Tbool
  (* binary operators on integers *)
  | Plus(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tint else Tunit
  | Mult(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tint else Tunit
  (* binary operators on booleans *)
  | And(e1,e2) -> if(((obtainType g e1) = Tbool) && ((obtainType g e2) = Tbool)) then Tbool else Tunit
  | Or(e1,e2) -> if(((obtainType g e1) = Tbool) && ((obtainType g e2) = Tbool)) then Tbool else Tunit
  (* comparison operations on integers *)
  | Cmp(e1) -> if(((obtainType g e1) = Tint)) then Tbool else Tunit
  (* a conditional expression *)
  | If_Then_Else(e0,e1,e2) -> if (((obtainType g e0) = Tbool ) && ((obtainType g e1)=(obtainType g e2)))  then (obtainType g e1) else Tunit



  | Lambda(e3,t,e1) -> (


  match e3 with 

   V(x) -> (let l = (findListPos [] x e1 g) in (

        if l=[] then Tfunc(t,(obtainType (appendTables [(x,t)] g) e1))
        else( 

            if (allEq l) then (

                  if (t = (findeq l)) then  Tfunc((findeq l),(obtainType (appendTables [(x,(findeq l))] g) e1))
                  else Tunit


            )
         else  Tunit 

        )
      )
   )

  | _ -> Tunit

)

  | App(e1,e2) -> let t12 = (obtainType g e1) and t1 = (obtainType g e2) in (


    match t12 with 

    Tfunc(t3,t4) -> if (t3=t1) then t4 else Tunit
    | _ -> Tunit

  )





(* hastype : ((string * exptype) list) -> exptree -> bool *)
let rec hastype g e = match e with

   V(s) ->  if (presentIn s g) then (true)else false
  | Integer(i) -> true
  | Bool(b) -> true
 (* binary operators on integers *)
  | Plus(e1,e2) -> if((hastype g e1) && (hastype g e2) && ((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then true else false
  | Mult(e1,e2) -> if((hastype g e1) && (hastype g e2) && ((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then true else false
  (* binary operators on booleans *)
  | And(e1,e2) -> if((hastype g e1) && (hastype g e2) && ((obtainType g e1) = Tbool) && ((obtainType g e2) = Tbool)) then true else false
  | Or(e1,e2) -> if((hastype g e1) && (hastype g e2) && ((obtainType g e1) = Tbool) && ((obtainType g e2) = Tbool)) then true else false
  (* comparison operations on integers *)
  | Cmp(e1) -> if((hastype g e1)  && ((obtainType g e1) = Tint)) then true else false
  (* a conditional expression *)
  | If_Then_Else(e0,e1,e2) -> if ((hastype g e0) && (hastype g e1) && (hastype g e2) && ((obtainType g e0)=Tbool) && ((obtainType g e1)=(obtainType g e2))) then true else false 

  | Lambda(e3,t3,e1) ->(

    match e3 with

       V(x) ->  let g1 = (appendTable g x t3)  in (hastype g1 e1)
     | _ -> false

  ) 


  | App(e1,e2) -> let t1 = (obtainType g e1) and t2=(obtainType g e2) in ( match t1 with

  
                    Tfunc(t3,t4) -> if ((hastype g e1) && (hastype g e2) && (t3=t2)) then true else false
                    | _ -> false

                  )

  







(* Now the Eval, Compile, Stackmc*)

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
  





(* The Krivine Machine, using a stk_type list as a substitute to opcodes for representing commands like And, Add etc. in the functional world*)

let rec eval cl stk stk_type= match cl with 

 Clos(ex,gamma) ->  (match ex with
    V(s) ->  

    if stk_type=[] then  (if (presentIn1 s gamma) then (let p = (findIn1 s gamma) in 


      match p with 
      Clos(e2,gamma) -> (eval p stk stk_type)
  | _ -> p

    ) else (raise (Myexp "Variable not present in the table"))) 


  else(

       let rec evalType stk_type stk = (

          match stk_type with 

          [] -> (

            match stk with 

            [] -> raise (Myexp "Improper stack formation")
            | x :: xs -> x

          )

          | y :: ys -> (

            if y = "Add" then (match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i+j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")
            )
             else if y = "Mult" then (

              match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i*j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

             )
               else if y = "And" then (

                match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i&&j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

               )

              else if y = "Or" then (
              
                match stk with

                 x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i||j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")


              ) 

            else (

               match stk with

               x0 ::  x1 :: x2 :: xs -> (

                match x0 with 

                ValueClosure(BoolVal(true),_) -> (evalType ys ((eval x1 xs []) ::xs))
                | ValueClosure(BoolVal(false),_) -> (evalType ys ((eval x2 xs []) ::xs))
                | _ -> raise(Myexp "Improper stack formation")

               )

              | _ -> raise (Myexp "Improper stack formation")



            )

          )

       ) in (

       if (presentIn1 s gamma) then (

        let p = (findIn1 s gamma) in (

          match  p with
           Clos(ex2,gamma) -> evalType stk_type ((eval p [] []) :: stk)
          | _ -> evalType stk_type (p :: stk)


        )


       )

     else raise (Myexp "Variable not present in the table")

    )
  )


  | Integer(i) -> if stk_type=[] then  ValueClosure(NumVal(i),gamma) else(

      let rec evalType stk_type stk = (

          match stk_type with 

          [] -> (

            match stk with 

            [] -> raise (Myexp "Improper stack formation")
            | x :: xs -> x 

          )

          | y :: ys -> (

            if y = "Add" then (match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i+j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")
            )
             else if y = "Mult" then (

              match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i*j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

             )
               else if y = "And" then (

                match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i&&j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

               )

              else if y = "Or" then (
              
                match stk with

                 x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i||j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")


              ) 

            else (

               match stk with

               x0 ::  x1 :: x2 :: xs -> (

                match x0 with 

                ValueClosure(BoolVal(true),_) -> (evalType ys ((eval x1 xs []) ::xs))
                | ValueClosure(BoolVal(false),_) -> (evalType ys ((eval x2 xs []) ::xs))
                | _ -> raise(Myexp "Improper stack formation")

               )

              | _ -> raise (Myexp "Improper stack formation")



            )


          )

       ) in (evalType stk_type ((ValueClosure(NumVal(i),gamma))::stk))


  )

  | Bool(true) -> if stk_type = [] then ValueClosure(BoolVal(true),gamma) else (


          let rec evalType stk_type stk = (

          match stk_type with 

          [] -> (

            match stk with 

            [] -> raise (Myexp "Improper stack formation")
            | x :: xs -> x

          )

          | y :: ys -> (

            if y = "Add" then (match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i+j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")
            )
             else if y = "Mult" then (

              match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i*j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

             )
               else if y = "And" then (

                match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i&&j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

               )

              else if y = "Or" then (
              
                match stk with

                 x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i||j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")


              ) 

            else (

               match stk with

               x0 ::  x1 :: x2 :: xs -> (

                match x0 with 

                ValueClosure(BoolVal(true),_) -> (evalType ys ((eval x1 xs []) ::xs))
                | ValueClosure(BoolVal(false),_) -> (evalType ys ((eval x2 xs []) ::xs))
                | _ -> raise(Myexp "Improper stack formation")

               )

              | _ -> raise (Myexp "Improper stack formation")



            )


          )

       ) in (evalType stk_type ((ValueClosure(BoolVal(true),gamma))::stk))



  )


  | Bool(false) -> if stk_type = [] then ValueClosure(BoolVal(false),gamma) else(


     let rec evalType stk_type stk = (

          match stk_type with 

          [] -> (

            match stk with 

            [] -> raise (Myexp "Improper stack formation")
            | x :: xs -> x 

          )

          | y :: ys -> (

            if y = "Add" then (match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i+j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")
            )
             else if y = "Mult" then (

              match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i*j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

             )
               else if y = "And" then (

                match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i&&j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

               )

              else if y = "Or" then (
              
                match stk with

                 x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i||j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")


              ) 

            else (

               match stk with

               x0 ::  x1 :: x2 :: xs -> (

                match x0 with 

                ValueClosure(BoolVal(true),_) -> (evalType ys ((eval x1 xs []) ::xs))
                | ValueClosure(BoolVal(false),_) -> (evalType ys ((eval x2 xs []) ::xs))
                | _ -> raise(Myexp "Improper stack formation")

               )

              | _ -> raise (Myexp "Improper stack formation")



            )


          )

       ) in (evalType stk_type ((ValueClosure(BoolVal(false),gamma))::stk))


  )

  | Plus(ex1, ex2) ->  eval (Clos(ex2,gamma))  ((Clos(ex1,gamma)) :: stk) ("Add" :: stk_type)  (*let ValueClosure(NumVal(y1),alpha) = (eval (Clos(ex1,gamma)) stk) and ValueClosure(NumVal(y2),beta) = (eval (Clos(ex2,gamma)) stk) in ValueClosure(NumVal(y1 + y2),gamma)*)
  | Mult(ex1, ex2) ->  eval (Clos(ex2,gamma))  ((Clos(ex1,gamma)) :: stk) ("Mult" :: stk_type)     (*let ValueClosure(NumVal(y1),alpha) = (eval (Clos(ex1,gamma)) stk) and ValueClosure(NumVal(y2),beta) = (eval (Clos(ex2,gamma)) stk) in ValueClosure(NumVal(y1 * y2),gamma)*)

  | And(ex1,ex2) -> eval (Clos(ex2,gamma))  ((Clos(ex1,gamma)) :: stk) ("And" :: stk_type) (*let ValueClosure(BoolVal(b1),alpha) = (eval (Clos(ex1,gamma)) stk) and ValueClosure(BoolVal(b2),beta) = (eval (Clos(ex2,gamma)) stk) in ValueClosure(BoolVal(b1 && b2),gamma)*)
  | Or(ex1,ex2) -> eval (Clos(ex2,gamma))  ((Clos(ex1,gamma)) :: stk) ("Or" :: stk_type)  (*let ValueClosure(BoolVal(b1),alpha) = (eval (Clos(ex1,gamma)) stk) and ValueClosure(BoolVal(b2),beta) = (eval (Clos(ex2,gamma)) stk) in ValueClosure(BoolVal(b1 || b2),gamma)*)  

  | Cmp(ex1) -> if stk_type = [] then (let ValueClosure(NumVal(y1),alpha) = (eval (Clos(ex1,gamma)) stk stk_type) in ValueClosure(BoolVal(cmp (mk_big y1)),gamma))
                  else (

                           let rec evalType stk_type stk = (

          match stk_type with 

          [] -> (

            match stk with 

            [] -> raise (Myexp "Improper stack formation")
            | x :: xs -> x 

          )

          | y :: ys -> (

            if y = "Add" then (match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i+j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")
            )
             else if y = "Mult" then (

              match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(NumVal(i),gamma) = x1 and ValueClosure(NumVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(NumVal(i*j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

             )
               else if y = "And" then (

                match stk with 

              x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i&&j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")

               )

              else if y = "Or" then (
              
                match stk with

                 x1 :: x2 :: xs -> (let ValueClosure(BoolVal(i),gamma) = x1 and ValueClosure(BoolVal(j),beta) = (eval x2 [] [] ) in (evalType ys ((ValueClosure(BoolVal(i||j),beta)) :: xs)))
              | _ -> raise (Myexp "Improper stack formation")


              ) 

            else (

               match stk with

               x0 ::  x1 :: x2 :: xs -> (

                match x0 with 

                ValueClosure(BoolVal(true),_) -> (evalType ys ((eval x1 xs []) ::xs))
                | ValueClosure(BoolVal(false),_) -> (evalType ys ((eval x2 xs []) ::xs))
                | _ -> raise(Myexp "Improper stack formation")

               )

              | _ -> raise (Myexp "Improper stack formation")



            )


          )

       ) in  (let ValueClosure(NumVal(y1),alpha) = (eval (Clos(ex1,gamma)) stk []) in (evalType (stk_type) (ValueClosure(BoolVal(cmp (mk_big y1)),gamma) :: stk)) )



                  )

  | If_Then_Else(ex1,ex2,ex3) -> eval (Clos(ex1,gamma)) ((Clos(ex2,gamma)):: (Clos(ex3,gamma)) :: stk) ("IFTE" :: stk_type)

  | Lambda(e3,t,e') -> (

    match e3 with 

    V(x) -> (

      match stk with 

      [] -> raise (Myexp "Improper stack formation3")
      | l :: ls ->(
        let alpha = (eval (Clos(e',(appendTable1 gamma x l))) ls stk_type) in (

          match alpha with 
          ValueClosure(a,b) -> ValueClosure(a,gamma)

        )

      ) 

    )


    | _ -> (raise (Myexp "Invalid function formation"))

  )

  | App(e1,e2) -> (eval (Clos(e1,gamma)) ((Clos(e2,gamma)) :: stk) stk_type)


)



(*Krivine machine*)
let krivine g e gamma stk stk_type= let cl = Clos(e,gamma) in ((if (hastype g e) then (eval cl stk stk_type) else (raise (Myexp "Type failure"))))



(*Post order traversal*)
let rec compileTemp ex = match ex with

    V(s) -> [VAR(s)]
  | Integer(i) ->[NCONST(mk_big i)]
  | Bool(true) -> [BCONST(true)]
  | Bool(false) -> [BCONST(false)]
  | Plus(ex1, ex2) -> (appendList (compileTemp ex1) (compileTemp ex2)) @ [PLUS]
  | Mult(ex1, ex2) -> (appendList (compileTemp ex1) (compileTemp ex2)) @ [MULT]
  | And(ex1,ex2) -> (appendList (compileTemp ex1) (compileTemp ex2)) @ [CONJ]
  | Or(ex1,ex2) -> (appendList (compileTemp ex1) (compileTemp ex2)) @ [DISJ]
  | Cmp(ex1) -> (compileTemp ex1) @ [CMP]
  | If_Then_Else(ex1,ex2,ex3) -> (compileTemp ex1) @ [COND((compileTemp ex2),(compileTemp ex3))]
  | Lambda(ex3,t,ex1) -> (

    match ex3 with 

     V(x) -> [CLOS(x,(compileTemp ex1) @ [RET])]
     | _ -> []

  )

  | App(e1,e2) -> (compileTemp e1) @ (compileTemp e2) @ [APP]

let compile ex g = if (hastype g ex) then (compileTemp ex) else (raise (Myexp "Type failure"))

(*SECD machine using the opcodes from the compile function*)
let rec secd stk binding pgm dump =  match pgm with 

   [] -> (

    match stk with
      x :: xs -> x
    |  _ ->  raise (Myexp "Illformed stack")

   )

 | VAR(s) :: ys ->  if (presentIn1 s binding) then (secd ((findIn1 s binding) :: stk) binding ys dump) else (raise (Myexp "Variable not present in the binding"))
 | NCONST(i) :: ys -> (secd (Num(i) :: stk) binding ys dump)
 | BCONST(true) :: ys -> secd (Bool(true) :: stk) binding ys dump
 | BCONST(false) :: ys -> secd (Bool(false) :: stk) binding ys dump


 | PLUS :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> secd (Num(add x2 x1) :: xs) binding ys dump
            |   _ -> raise (Myexp "Illformed stack")


 )


 | MULT :: ys -> (

      match stk with 
              Num(x1) :: (Num(x2) :: xs) -> secd (Num(mult x2 x1) :: xs) binding ys dump
            |   _ -> raise (Myexp "Illformed stack")


 )

 
 | CONJ :: ys ->(

      match stk with 
              Bool(x1) :: (Bool(x2) :: xs) -> secd (Bool(x2 && x1) :: xs) binding ys dump
            |   _ -> raise (Myexp "Illformed stack")


 )

 | DISJ :: ys ->(

      match stk with 
              Bool(x1) :: (Bool(x2) :: xs) -> secd (Bool(x2 || x1) :: xs) binding ys dump
            |   _ -> raise (Myexp "Illformed stack")


 )

 | CMP :: ys -> (

     match stk with
       Num(x1) :: xs -> secd (Bool(cmp x1) :: xs) binding ys dump
     | _ -> raise (Myexp "Illformed stack")

 )

 | COND(y1,y2) :: ys -> (

     match  stk with
      Bool(true) :: xs -> secd xs binding (y1 @ ys) dump
     | Bool(false) :: xs -> secd xs binding (y2 @ ys) dump
     | _ -> raise (Myexp "Illformed stack")
 )

| CLOS(x,c') :: ys -> secd ((ValClosure(x,c',binding)) :: stk) binding ys dump 

| APP :: ys -> (

  match stk with 

    a2 :: (ValClosure(x,c',otherGamma) :: xs) -> secd [] (appendTable1 otherGamma x a2) c' ((xs,binding,ys) :: dump)
  | _ -> raise (Myexp "Illformed stack")
)

| RET :: ys -> (

  match stk with 

  a :: s' -> (

    match dump with 

     (s,gamma,c') :: d -> secd (a :: s) gamma c' d 
    | _ -> raise (Myexp "Illformed stack")
  )

 | _ -> raise (Myexp "Illformed stack")
)
