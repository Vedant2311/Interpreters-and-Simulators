(* Integers *)
type bigint = sign * int list
  and sign = Neg | NonNeg

(* Arithmetic operations:  *)
(* Addition *)
val add: bigint -> bigint -> bigint
(* Multiplication *)
val mult: bigint -> bigint -> bigint
(* Comparision *)
val cmp:  bigint  -> bool

(* Functions to present the result in the form of a string. *)
val print_num: bigint -> string

(* Conversion functions from OCaml int to bigint. *)
val mk_big:  int -> bigint
