type bigint = sign * int list
    and sign = Neg | NonNeg


  exception Myexp of string

(* Getting the sign of the BigInt*)
  let valSign x = match x with
    (Neg, _)  -> Neg
  | (NonNeg, _) -> NonNeg
 
(* Getting the int list corresponding to the Big int*)
  let valList x = match x with
    (Neg, list_x) -> list_x
  | (NonNeg, list_x) -> list_x
 
(*Length of the int list*)
  let rec length l= match l with
     [] -> 0
  |  x::xs -> 1 + length(xs)
  
 (* Reversing the int list*)
  let rec revList l = match l with
      [] -> []
    |   x::xs -> (revList xs) @ [x]
(*Removing the first zeros of the list*)
let rec remZeros l = match l with 
            [] -> []
        |  x::xs -> if x=0 then remZeros xs else l
(* Finding the Head of the list and returning -1 for an improper input*)
  let head l = match l with 
        [] -> -1
    |   x::xs -> x
 (* Finding the tail of the list and returning [] for the empty list case*)
   let tail l = match l with 
        [] -> []
      | x::xs -> xs




 (* Changing the Sign of the Big int*)
  let minus x = let changeSign (x:sign): sign = if x=NonNeg then Neg else NonNeg and listvalue = valList x and signValue = valSign x
                    in (if listvalue=[] then (NonNeg,[]) else (changeSign signValue, listvalue))

 (* To convert the given integer into the Big int in log(n) steps*)
  let mk_big x = let signValue = (if x<0 then Neg else NonNeg) and 
                    listVal = (

                      match x with
                        0 -> []
                      |   x -> (

                          let rec makeList x = match x with

                            0 -> []
                          |   x -> (

                              let beta = x mod 10 and gamma = x/10 in (makeList gamma) @ [beta]

                          )

                          and temp = (if x>=0 then x else -x) in makeList temp

                        )

                    )    in (signValue, listVal)

    
    (* To print the big integer as a string by concatenating the values of the list recursively into the output string as well as indicating the Minus sign if necessary*)
    let print_num x = let rec list2String l = match l with
                              | [] -> ""
                              | x::xs -> (string_of_int x) ^ (list2String xs)


                  and signValue = valSign x and listValue = valList x in 
                        (if signValue=Neg && listValue !=[] then 


                          ("-" ^ (list2String listValue) ) else if listValue==[] then "0" else (list2String listValue) 

                        )
 (* The absolute value of the integer: i.e changing the sign of the big int to NonNeg in any case*)
  let abs x = let listvalue = valList x in (NonNeg,listvalue) 


  (* Comparision operators*)
    
    (* The equality operator comparing the signs and the corresponding digits recursively*)
  let rec eq x y = let sign_x = valSign x and sign_y = valSign y 
            in  (

              if(sign_x != sign_y) then false

              else (

                let list_x = valList x and list_y = valList y in 
                  (

                    match list_x with 
                      [] -> (

                        match list_y with 
                          [] -> true
                        |   _ -> false

                      )

                    |   x::xs -> (
                    

                        match list_y with 
                          [] -> false
                        |   y::ys -> (

                          if x=y then eq (sign_x,xs) (sign_y,ys)

                          else  false
                        )

                    ) 

                  )

              )


            ) 

   (* The greater than Operator which compares the digits from the Most significant position recursively*)
    let rec gt x y = let sign_x = valSign x and sign_y = valSign y 
             in  (

              if((sign_x = Neg) && (sign_y = NonNeg)) then false

              else if ((sign_x = NonNeg) && (sign_y = Neg)) then true

              else if (sign_x = NonNeg) then (
                let list_x = valList x and list_y = valList y in (

                  if (length list_x) < (length list_y) then false
                else if (length list_x) > (length list_y) then true
                else(

                  match list_x with 
                      [] -> false

                    |   x::xs -> (
                    

                        match list_y with 
                              [] -> false
                        |    y::ys -> (

                          if x>y then  true
                        else if x<y then  false 

                          else  gt (sign_x, xs) (sign_y, ys)
                        )

                    ) 


                )

                )               

                
              )

              else (

                let list_x = valList x and list_y = valList y in (

                  if (length list_x) < (length list_y) then true
                else if (length list_x) > (length list_y) then false
                else(

                  match list_x with 
                      [] -> false

                    |   x::xs -> (
                    

                        match list_y with 
                           [] -> false
                        |     y::ys -> (

                          if x<y then  true
                        else if x>y then  false 

                          else  gt (sign_x, xs) (sign_y, ys)
                        )

                    ) 


                ) 

                )               

            ) 

            ) 
(* The Less than Operator which compares the digits from the Most significant position recursively*)
  let rec lt x y = let sign_x = valSign x and sign_y = valSign y 
             in  (

              if((sign_x = NonNeg) && (sign_y = Neg)) then false

              else if ((sign_x = Neg) && (sign_y = NonNeg)) then true

              else if (sign_x = NonNeg) then (
                let list_x = valList x and list_y = valList y in (

                  if (length list_x) < (length list_y) then true
                else if (length list_x) > (length list_y) then false
                else(

                  match list_x with 
                      [] -> false

                    |   x::xs -> (
                    

                        match list_y with 
                           [] -> false
                      |     y::ys -> (

                          if x>y then  false
                        else if x<y then  true 

                          else  lt (sign_x, xs) (sign_y, ys)
                        )

                    ) 


                )

                )               

                
              )

              else (

                let list_x = valList x and list_y = valList y in (

                  if (length list_x) < (length list_y) then false
                else if (length list_x) > (length list_y) then true
                else(

                  match list_x with 
                      [] -> false

                    |   x::xs -> (
                    

                        match list_y with 
                        
                              []  -> false  
                        |    y::ys -> (

                          if x<y then  false
                        else if x>y then  true 

                          else  lt (sign_x, xs) (sign_y, ys)
                        )

                    ) 


                ) 

                )               

            ) 

            )

  (* (x>=y) = !(x<y)*)
    let geq x y = let bolVal = lt x y in (not bolVal)

(* (x<=y) = !(x>y)*)
  let leq x y = let bolVal = gt x y in (not bolVal)   

 (* For adding the digits with a carry and returning the carry and the final output*)
  let addc a b c = if a+b+c>=10 then (a+b+c-10, 1) else (a+b+c,0)   (* Value, Carry*)

 (* Adding two lists with a carry using the addc function for all the digits in the list, assuming that both the lists have the same sign which comes under a conditon in the calling function*)
  let rec addList l1 l2 c = match l1 with 
                 [] -> (

                    match l2 with 

                      [] -> (

                        if c=1 then [1] else []

                      )

                    |   y::ys -> (

                        let (value,carry) = addc 0 y c in (value :: (addList [] ys carry))

                      ) 

                    

                 )

           |    x::xs -> (

                match l2 with 

                      [] -> (

                        let (value,carry) = addc x 0 c in (value :: (addList [] xs carry))

                      )

                    |   y::ys -> (

                        let (value,carry) = addc x y c in (value :: addList xs ys carry) 


                      )

                    

              )  

 (* Subtracting two lists l1 and l2 with l1 to be greater than l2 which is a condition in the calling function*)
  let rec subList l1 l2 = match l1 with 
           [] ->  (

              match l2 with 

                [] -> []
              |   _ -> raise (Myexp "The operation is not performed properly")

              

           )

      |     x::xs -> (

            match l2 with 

                 [] ->  (if x>=0 then l1 else (           (* THIS CASE WAS MISSING IN THE PREVIOUS CODE *)

                    match xs with 

                  [] -> raise (Myexp "The operation is not performed properly 1")
                |   x1::xs1 -> (

                    let x2 = x1-1 in let xs2 = x2::xs1 in (x+10)::(subList xs2 [])
                       
                  )
                



                 )

               )
            |   y::ys -> if (x-y<0) then (

                match xs with 

                  [] -> raise (Myexp "The operation is not performed properly 1")
                |   x1::xs1 -> (

                    let x2 = x1-1 in let xs2 = x2::xs1 in (x-y+10)::(subList xs2 ys)
                       
                  )
                


              )

                else (x-y) :: (subList xs ys)

            


          )


   (* The general Add list function segregaring the cases of addition and substraction as per the cases of the signs of the Big ints which are taken from the calling function*)
  let addl l1 s1 l2 s2 c= if s1=s2 then (s1, revList (addList l1 l2 c))

                  else if (eq (NonNeg,l1) (NonNeg,l2)) then (NonNeg,[])

                   else if s1=Neg then (

                    if gt (NonNeg,revList l1) (NonNeg, revList l2) then (Neg, revList (subList l1 l2) )
                    
                      else (NonNeg, revList (subList l2 l1))
                  ) 

                  else (
                    if (gt (abs(NonNeg,revList l1)) (abs(Neg, revList l2))) then (NonNeg, revList (subList l1 l2) )
                      else (Neg, revList (subList l2 l1))

                  )

  
  
 (* The add function for Big ints using addl function*)
  let add a b = let sign_a = valSign a and sign_b = valSign b and list_a = valList a and list_b = valList b in let
              rev_a = revList list_a and rev_b = revList list_b in let (outSign, outList) = addl rev_a sign_a rev_b sign_b 0 in (outSign,remZeros outList)       (* ADDED THE remZeros funcion*)

  (* The subtract function for Big ints by adding a with (Minus b)*)
  let sub a b = add a (minus b)

  (* Multiplying two digits a and b with an incoming carry and giving out the value and the corresponding carry*)
  let multc a b c = ((a*b + c) mod 10, (a*b +c)/10)   (* Val, carry*)

 (* Multplying a list with a digit, which is an elementary step in the conventional multiplication method*)
  let rec multListdigit l1 x c carry= match l1 with 
              [] -> (

                  if carry!=0 then [carry]
                    else []
              )

            | y::ys -> (

                 
                 if(c>0) then 0:: (multListdigit l1 x (c-1) carry)
               else let (valoo,carr) = multc y x carry in valoo::(multListdigit ys x 0 carr)
              

            )
 (*Multiplying the lists using the multc function by recursively accessing the elements of the lists using the elementary multiplication method of shifting and adding the results*)
  let rec multList l1 l2 c= match l2 with 
        [] -> []
      | y::ys -> (addList (multListdigit l1 y c 0) (multList l1 ys (c+1)) 0)

 (* Multiplying the two big ints using the multList function*)
  let mult a b = let sign_a = valSign a and sign_b = valSign b and list_a = valList a and list_b = valList b in 
                     
                        let signValue = (if sign_a = sign_b then NonNeg else Neg) in (signValue, revList (multList (revList list_a) (revList list_b) 0))
 (* Take the first 'len' values of the list l*) 
  let rec genFirst l len = match l with 
              [] -> []
          |   x::xs -> (if len>0 then x::(genFirst xs (len-1)) else []);;

 (* Givenn two lists, generates the quotient as a single digit, as is done in elementary division, along with returning the remainder as well*)
  let genQuo l4 l2 =  if eq (mult (NonNeg,l2) (NonNeg,[1])) (NonNeg,l4) then (1,[])
                        else if gt (mult (NonNeg,l2) (NonNeg,[2])) (NonNeg,l4) then (1,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[1]))))
                          else if eq (mult (NonNeg,l2) (NonNeg,[2])) (NonNeg,l4) then (2,[])
    (* Format : Q,R *)          else if gt (mult (NonNeg,l2) (NonNeg,[3])) (NonNeg,l4) then (2,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[2]))))
                          else if eq (mult (NonNeg,l2) (NonNeg,[3])) (NonNeg,l4) then (3,[])
                              else if gt (mult (NonNeg,l2) (NonNeg,[4])) (NonNeg,l4) then (3,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[3]))))
                          else if eq (mult (NonNeg,l2) (NonNeg,[4])) (NonNeg,l4) then (4,[])
                              else if gt (mult (NonNeg,l2) (NonNeg,[5])) (NonNeg,l4) then (4,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[4]))))
                          else if eq (mult (NonNeg,l2) (NonNeg,[5])) (NonNeg,l4) then (5,[])
                              else if gt (mult (NonNeg,l2) (NonNeg,[6])) (NonNeg,l4) then (5,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[5]))))
                          else if eq (mult (NonNeg,l2) (NonNeg,[6])) (NonNeg,l4) then (6,[])
                              else if gt (mult (NonNeg,l2) (NonNeg,[7])) (NonNeg,l4) then (6,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[6]))))
                          else if eq (mult (NonNeg,l2) (NonNeg,[7])) (NonNeg,l4) then (7,[])
                              else if gt (mult (NonNeg,l2) (NonNeg,[8])) (NonNeg,l4) then (7,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[7]))))
                          else if eq (mult (NonNeg,l2) (NonNeg,[8])) (NonNeg,l4) then (8,[])
                              else if gt (mult (NonNeg,l2) (NonNeg,[9])) (NonNeg,l4) then (8,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[8]))))
                          else if eq (mult (NonNeg,l2) (NonNeg,[9])) (NonNeg,l4) then (9,[])
                              else (9,valList (sub (NonNeg,l4) (mult (NonNeg,l2) (NonNeg,[9]))))

 (* Appends l1 and l2*)
  let rec appendList l1 l2 = match l1 with 
                  [] -> l2
              |   x::xs -> x :: (appendList xs l2)
 (* Generates the list of the values of l after the (len)th index*)
  let rec getAfterGen l len = match l with 
            [] -> []
        |   x::xs -> if len=0 then l else getAfterGen xs (len-1)

  
 
                                         

 (* Finds the remainder of the division of l1/l2 without taking care of the extra zeroes to be padded for getting the right quotient but is not necessary for the remainder*)
  let rec remList l1 l2 len = if l2=[] then raise (Myexp "Division by zero") 
                                else if l1=[] then []
                              else if (length l2) > (length l1) then l1
                               else if gt (NonNeg,l2) (NonNeg,l1) then l1 
                                 else let l3 = genFirst l1 len in let l4 = (if geq (NonNeg,l3) (NonNeg,l2) then l3 else (genFirst l1 (len+1))) in
                                        let (quodig,l6) = genQuo l4 l2 in let l5 = appendList l6 (getAfterGen l1 (length l4))  
                                                    in let finalOut =( remList (remZeros l5) l2 len

                                                                     ) in finalOut
 (* the final remainder function implementing remList function*)
  let rem a b = let sign_a = valSign a and list_a = valList a and list_b = valList b in
                        let signValue = sign_a in (signValue, remZeros (remList list_a list_b (length list_b)))


  
  (**
  
    The Following is the other method implemented by me for the division function, which is different from the one which computes remainder since in that method, I faced an error of extra padding

  **)


(* Returns (Quo, Rem) by the basic method of repeated subtraction until l2>=l1 in l1/12 *)
  let rec normalDiv l1 l2 = 
      if gt (NonNeg, remZeros l2) (NonNeg, remZeros l1) then (0,l1)
         else if l2 = [] then raise (Myexp "Divison by Zero")
           else if eq (NonNeg, remZeros l1) (NonNeg, remZeros l2) then (1,[])
              else (

                  let (q,r) = normalDiv (remZeros (valList (sub (NonNeg, l1) (NonNeg, l2)))) l2 in 
                      (q+1,r)

              )

(* Looping the elementary division method*)

  let loopDiv l1 l2 = let rec loop l1 q r = (

                            if (l1 = []) then (q,r)
                              else (
                                    let (q1,r1) = (normalDiv (remZeros (r @ [head l1])) l2) in 
                                          loop (tail l1) (q1 :: q) r1   

                              )                               
                        )  in loop l1 [0] []

  
  (*Returns the Quotient and Remainder. Have to deal with remainder in this function as well since it is important to obtain the remainder in the process of calculating the quotient by this method*)
 let divList l1 l2 = let (q,r) = loopDiv l1 l2 in q  

  (* The division function*)
  let div a b = let sign_a = valSign a and sign_b = valSign b and list_b = valList b and list_a = valList a in
            let signValue = (if sign_a = sign_b then NonNeg else Neg) in let l7 = (divList list_a list_b) in 
                                          (signValue, remZeros ( revList l7))
