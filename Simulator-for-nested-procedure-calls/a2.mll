{
  open A3
  exception Not_implemented
   exception InvalidToken of string
   exception Eof
}

(*
  Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a5.ml)
*)


let whitespace = [' ' '\t' '\n']

(* Defining regexps for valid and invalid integer expressions*)

let digiet = ['0'-'9']
let digit1 = ['1' - '9']
let zero = '0'
let sign = '-'

let integer = zero | (sign? digit1 digiet*) 
let errorInteger1 = sign? zero digiet+ 

(* Defining regexps for variable names and procedure names*)

let lowerCases = ['a' - 'z']
let upperCases = ['A' - 'Z']

let variable = lowerCases+
let procedure = upperCases+


(* For other misc parameters *)

let leftParen = '('
let rightParen = ')'	

let colon = ':'
let eq = '='
let return = "return"

(* Rules section*)

rule read = parse
  
  errorInteger1 as s {raise ( InvalidToken (String.concat " " ["Invalid integer";s]))}
| integer as i {INT (int_of_string i) }

| eq {EQ}
| colon {COLON}
| return {RET}

| "show" {SHOW}

| procedure as s {PROC(s)}
| variable as s {VAR(s)}


| leftParen {LP}
| rightParen {RP}


| "," {COMMA}
| ";" {EOL}


| whitespace                    {read lexbuf}
| eof   {EOF}
| _  as s{raise (InvalidToken (String.concat " " ["Invalid character";String.make 1 s]))}


