open A1
exception Not_implemented
exception Myexp of string

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


let appendTable g x t1 = let yY = (x,t1) in yY::g

let rec appendTables p g = match p with 

	[] -> g
	| x::xs -> x :: (appendTables xs g)

let rec removeThis str g = match g with 

  [] -> []
  | (x,t) :: xs -> (if (x=str) then (removeThis str xs) else ( (x,t) :: (removeThis str xs)) ) 


 let appendTableParallel p g = match  p  with
  [(str, t)]   -> let rec typeThere str1 g = (

    match g with 
    [] -> Tunit
   | x :: xs -> (match  x with
                 (y,q) -> (if (y=str1) then q else (typeThere str1 xs))
                 
                )


  )  in ( 


  let   rec isThere str1 g = (

     match g with 
    [] -> false
   | x :: xs -> (match  x with
                 (y,q) -> (if (y=str1) then true else (isThere str1 xs))
                 
                )



  ) in




    (if (isThere str g) then (if (t = (typeThere str g)) then ( p @ (removeThis str g)) else (removeThis str g) ) else p @ g )


)

 
 | [] -> []



let appendTableSequence p g = match  p  with
  [(str, t)]   -> let rec typeThere str1 g = (

    match g with 
    [] -> Tunit
   | x :: xs -> (match  x with
                 (y,q) -> (if (y=str1) then q else (typeThere str1 xs))
                 
                )


  )  in (

    let rec isThere str1 g = (

     match g with 
    [] -> false
   | x :: xs -> (match  x with
                 (y,q) -> (if (y=str1) then true else (isThere str1 xs))
                 
                )



  ) in 



    (if (isThere str g) then (let alp = (typeThere str g) in (str,alp) :: (removeThis str g)) else p @ g )



   )

 | [] -> []

let rec allEq l = match l with 

	[] -> true
  | x -> true
  | x :: y :: xs -> if ((x=y) || (x=Tunit)) then (allEq (y::xs)) else false

let rec findeq l = match l with

	[] -> Tunit
  | x::xs -> if (x=Tunit) then (findeq xs) else x

let rec mklist s p = if p=0 then [] else (s :: (mklist s (p-1)))

let rec findIth i l = match l with 

 	x :: xs -> if i=1 then x else (findIth (i-1) l)

let rec findListPos s x e g= match e with 

	 Var (str) ->  if str=x then (

	 	match s with 

	 	[] -> [Tunit]		(* Means that f(x) = x*)
	 	| p:: xs -> if (p="Add") then [Tint]
	 					else(

	 						
	 							if p="And" then [Tbool]
	 						    else(

	 						    	if p="Tuple" then (

	 						    		let l = (findIn str g) in (

	 						    			match l with 

	 						    			Ttuple(q) -> [Ttuple(q)]
	 						    			| _ -> [Tunit]


	 						    		)
	 						    	)

	 						         else(
	 						         	[]

	 						         ) 

	 						    )
	 						)
	 					)

	 		else []



  | N(i) -> []
  | B(b) ->  []
  (* unary operators on integers *)
  | Abs(e1) -> findListPos ("Add" :: s) x e1 g
  | Negative(e1) -> (findListPos ("Add" :: s) x e1 g)
  (* unary operators on booleans *)
  | Not(e1) -> (findListPos ("And" :: s) x e1 g)
  (* binary operators on integers *)
  | Add(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  | Sub(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  | Mult(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  | Div(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  | Rem(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  (* binary operators on booleans *)
  | Conjunction(e1,e2) -> (findListPos ("And" :: s) x e1 g) @ (findListPos ("And" :: s) x e2 g)
  | Disjunction(e1,e2) -> (findListPos ("And" :: s) x e1 g ) @ (findListPos ("And" :: s) x e2 g)
  (* comparison operations on integers *)
  | Equals(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  | GreaterTE(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  | LessTE(e1,e2) -> (findListPos ("Add" :: s) x e1 g ) @ (findListPos ("Add" :: s) x e2 g)
  | GreaterT(e1,e2) -> (findListPos ("Add" :: s) x e1 g ) @ (findListPos ("Add" :: s) x e2 g )
  | LessT(e1,e2) -> (findListPos ("Add" :: s) x e1 g) @ (findListPos ("Add" :: s) x e2 g)
  (* expressions using parenthesis *)
  | InParen(e1) -> (findListPos s x e1 g) 
  (* a conditional expression *)
  | IfThenElse(e0,e1,e2) -> (findListPos ("And" :: s) x e0 g) @ (findListPos s x e1 g ) @ (findListPos s x e2 g)

  (* creating n-tuples (n >= 0) *)
  | Tuple(n,l) -> (

  	match l with 

  	[] -> []
  	| p::xs -> (findListPos s x p g) @ (findListPos s x (Tuple(n-1,xs)) g)


  )




  | Project((i,n),e1) -> (

  	if ((i<=n) && (i>=1)) then (

  		match e1 with

  		Tuple(p,l) -> (if (n=p) then (
  
  			match l with 

  			  [] -> [Tunit]
  			| x1  :: xs -> if i=1 then (findListPos (s) x x1 g) else (let e2 = Project((i-1,n-1),Tuple(p-1,xs)) in (findListPos s x e2 g))


  		) else [Tunit])

  		
  		| Var(q) -> if (q=x) then (findListPos ("Tuple" :: s) x e1 g) else []

  		| _ -> []
  	)


  	else  [Tunit]

  )

    (* COMMENT: Left from here *)

  | Let(d,e1) -> let rec obtainPosDef s x d g= (match d with 

	Simple(str,t,e2) -> if (not(str=x)) then (findListPos s x e2 g) else [t] @ (findListPos s x e2 g)
	| Local(d1,d2) -> (obtainPosDef s x d1 g) @ (obtainPosDef s x d2 g)
	| Parallel(l) -> (

		match l with 

		[] -> []
		| x1 :: xs -> (obtainPosDef s x x1 g) @ (obtainPosDef s x (Parallel (xs)) g)

	) 

	| Sequence(l) -> (

		match l with 

		[] -> []
		| x1::xs -> (obtainPosDef s x x1 g) @ (obtainPosDef s x (Sequence (xs)) g)

	)
)  in (obtainPosDef s x d g) @ (findListPos s x e1 g)


  | FunctionAbstraction(y,t,e1) -> (

  	if(x=y) then (

  		[t] @ (findListPos s x e1 g)

  	) else (findListPos s x e1 g)


  )



  | FunctionCall(e1,e2) -> (

  	match e1 with 

  	FunctionAbstraction(y,t,e3) -> if (x=y) then [t] @(findListPos s x e3 g) @ (findListPos s x e2 g) else (findListPos s x e3 g) @ (findListPos s x e2 g)
  	| _ -> [Tunit]

  )

  



let rec obtainType g e = match e with

	 Var (s) ->  if (presentIn s g) then (findIn s g) else Tunit
  | N(i) -> Tint
  | B(b) ->  Tbool
  (* unary operators on integers *)
  | Abs(e1) -> if((obtainType g e1) = Tint) then Tint else Tunit
  | Negative(e1) -> if((obtainType g e1) = Tint) then Tint else Tunit
  (* unary operators on booleans *)
  | Not(e1) -> if((obtainType g e1) = Tbool) then Tbool else Tunit
  (* binary operators on integers *)
  | Add(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tint else Tunit
  | Sub(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tint else Tunit
  | Mult(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tint else Tunit
  | Div(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tint else Tunit
  | Rem(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tint else Tunit
  (* binary operators on booleans *)
  | Conjunction(e1,e2) -> if(((obtainType g e1) = Tbool) && ((obtainType g e2) = Tbool)) then Tbool else Tunit
  | Disjunction(e1,e2) -> if(((obtainType g e1) = Tbool) && ((obtainType g e2) = Tbool)) then Tbool else Tunit
  (* comparison operations on integers *)
  | Equals(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tbool else Tunit
  | GreaterTE(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tbool else Tunit
  | LessTE(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tbool else Tunit
  | GreaterT(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tbool else Tunit
  | LessT(e1,e2) -> if(((obtainType g e1) = Tint) && ((obtainType g e2) = Tint)) then Tbool else Tunit
  (* expressions using parenthesis *)
  | InParen(e1) -> (obtainType g e1) 
  (* a conditional expression *)
  | IfThenElse(e0,e1,e2) -> if (((obtainType g e0) = Tbool ) && ((obtainType g e1)=(obtainType g e2)))  then (obtainType g e1) else Tunit

  (* creating n-tuples (n >= 0) *)
  | Tuple(n,l) -> let rec getTypeList g l = (match l with 

	[] -> []
  | x :: xs -> (obtainType g x) :: (getTypeList g xs)

)

in  (let y = (getTypeList g l) in Ttuple(y))
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project((i,n),e1) -> (

  	if ((i<=n) && (i>=1)) then (

  		match e1 with

  		Tuple(p,l) -> if (n=p) then (
  
  			match l with 

  			  [] -> Tunit
  			| x  :: xs -> if i=1 then (obtainType g x) else (let e2 = Project((i-1,n-1),Tuple(p-1,xs)) in (obtainType g e2))


  		) else Tunit

  		
  		| _ -> Tunit
  	)


  	else  Tunit

  )

  | Let(d,e1) -> let rec obtainYield g d = (match d with 

	  Simple(s,t,e2) -> if (t = (obtainType g e2)) then [(s, obtainType g e2)] else []
	| Local(d1,d2) -> let p = (obtainYield g d1) in (obtainYield (appendTables p g) d2)
	| Parallel(l) -> (

		match l with 

		[] -> []
		| x :: xs -> let p = (obtainYield g x) in (appendTableParallel p g) @ ((obtainYield g (Parallel(xs))))

	)

	| Sequence(l) -> (

		match l with 

		[] -> []
		| x::xs -> let p = (obtainYield g x) in (appendTableSequence p (obtainYield (appendTables p g) (Sequence(xs))))

	)
)  in (let y = obtainYield g d in (obtainType (appendTables y g) e1))




  | FunctionAbstraction(x,t,e1) -> let l = (findListPos [] x e1 g) in (

  if l=[] then Tfunc(t,(obtainType (appendTables [(x,(findeq l))] g) e1))
  else(	

    if (allEq l) then (

      if (t = (findeq l)) then  Tfunc((findeq l),(obtainType (appendTables [(x,(findeq l))] g) e1))
      else Tunit


    )
     else  Tunit 

  )
)


  | FunctionCall(e1,e2) -> let t12 = (obtainType g e1) and t1 = (obtainType g e2) in (


  	match t12 with 

  	Tfunc(t3,t4) -> if (t3=t1 || t3=Tunit) then t4 else Tunit
  	| _ -> Tunit

  )

let rec getTypeList g l = match l with 

	[] -> []
  | x :: xs -> (obtainType g x) :: (getTypeList g xs)

  

let rec obtainYield g d = match d with 

	Simple(s,t,e) -> if (t = (obtainType g e)) then [(s, t)] else []
	| Local(d1,d2) -> let p = (obtainYield g d1) in (obtainYield (appendTables p g) d2)
	| Parallel(l) -> (

		match l with 

		[] -> []
		| x :: xs -> let p = (obtainYield g x) in (appendTableParallel p (obtainYield g (Parallel(xs))))

	)

	| Sequence(l) -> (

		match l with 

		[] -> []
		| x::xs -> let p = (obtainYield g x) in (appendTableSequence p (obtainYield (appendTables p g) (Sequence(xs))))

	)



(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t = match e with

	 Var (s) ->  if (presentIn s g) then (let y = (findIn s g) in (if t=y then true else false))else false
  | N(i) -> if t=Tint then true else false
  | B(b) -> if t= Tbool then true else false
  (* unary operators on integers *)
  | Abs(e1) -> ((hastype g e1 t) && (t=Tint))
  | Negative(e1) -> ((hastype g e1 t) && (t=Tint))
  (* unary operators on booleans *)
  | Not(e1) -> ((hastype g e1 t) && (t=Tbool))
  (* binary operators on integers *)
  | Add(e1,e2) -> if((hastype g e1 t) && (hastype g e2 t) && (t=Tint)) then true else false
  | Sub(e1,e2) -> if((hastype g e1 t) && (hastype g e2 t) && (t=Tint)) then true else false
  | Mult(e1,e2) -> if((hastype g e1 t) && (hastype g e2 t) && (t=Tint)) then true else false
  | Div(e1,e2) -> if((hastype g e1 t) && (hastype g e2 t) && (t=Tint)) then true else false
  | Rem(e1,e2) -> if((hastype g e1 t) && (hastype g e2 t) && (t=Tint)) then true else false
  (* binary operators on booleans *)
  | Conjunction(e1,e2) -> if((hastype g e1 t) && (hastype g e2 t) && (t=Tbool)) then true else false
  | Disjunction(e1,e2) -> if((hastype g e1 t) && (hastype g e2 t) && (t=Tbool)) then true else false
  (* comparison operations on integers *)
  | Equals(e1,e2) -> if((hastype g e1 Tint) && (hastype g e2 Tint) && (t=Tbool)) then true else false
  | GreaterTE(e1,e2) -> if((hastype g e1 Tint) && (hastype g e2 Tint) && (t=Tbool)) then true else false
  | LessTE(e1,e2) -> if((hastype g e1 Tint) && (hastype g e2 Tint) && (t=Tbool)) then true else false
  | GreaterT(e1,e2) -> if((hastype g e1 Tint) && (hastype g e2 Tint) && (t=Tbool)) then true else false
  | LessT(e1,e2) -> if((hastype g e1 Tint) && (hastype g e2 Tint) && (t=Tbool)) then true else false
  (* expressions using parenthesis *)
  | InParen(e1) -> (hastype g e1 t)
  (* a conditional expression *)
  | IfThenElse(e0,e1,e2) -> if (not (hastype g e0 Tbool)) then false else (if((hastype g e1 t) && (hastype g e2 t)) then true else false)

  (* creating n-tuples (n >= 0) *)
  | Tuple(n,l) -> (match l with 

  	 [] -> (

  	 	match t with 

  	 	Ttuple(p) -> (

        
  	 		match p with 
  
         [] -> true
  	 		| _ -> false

  	 	)


  	 	| _ -> false

  	 )

  	 | x::xs -> (

  	 	match t with 

  	 	Ttuple(p) -> (

  	 		match p with 

  	 		[] -> false
  	 		| y::ys -> (

  	 			if (not(hastype g x y)) then false else (let t1 = Ttuple(ys) and e1 = Tuple(n-1,xs) in (hastype g e1 t1))


  	 		)

  	 	)

  	 	| _ -> false

  	 )

  )
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project((i,n),e1) -> (

  	if ((i>n) || (i<=0)) then false else (

  		let tempFun g e1 t =(

  			match e1 with

  		Tuple(p,l) -> if (n=p) then (

  			match l with 

  			[] -> false
  		|  x  :: xs -> if i=1 then (hastype g x t) else (let e2 = Project((i-1,n-1),Tuple(p-1,xs)) in (hastype g e2 t))


  		)  else false

  		| _ -> false

  	) 

  	and	tempFun2 g e1 t = (

  			let y = (obtainType g e1) in (

            match y with

            Ttuple(l) -> if n=(List.length l) then (let p =(findIth i l) in (if p=t then true else false)) else false

            | _ -> false


        ) 


  		) in (let alpha = (tempFun g e1 t) and beta = (tempFun2 g e1 t) in (alpha || beta))

  	)


  )

  | Let(d,e1) -> let g1 = (obtainYield g d) in (if (hastype (appendTables g1 g) e1 t) then true else false)
  | FunctionAbstraction(x,t3,e1) ->( match  t with
  		| Tfunc(t1,t2) -> if (t1 = t3) then (let g1 = (appendTable g x t1)  in (hastype g1 e1 t2)) else false
  		| _ -> false
  )


  | FunctionCall(e1,e2) -> let t1 = (obtainType g e2) in (if (hastype g e1 (Tfunc(t1,t))) then true else false)

  




(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
let rec yields g d g_dash = (g_dash = obtainYield g d)
