%{
    open A1
%}

/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END DEF LOCAL BACKSLASH DOT SEMICOLON COLON PARALLEL TUNIT TBOOL TINT EOF
%start def_parser exp_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%type <A1.exptype> type_parser /* Returns expression type */
%%

type_parser:
   tuple_type {$1}
 | type_parser MINUS GT tuple_type {Tfunc($1, $4)}
;

tuple_type:
 
 const_type {$1}
| LP inner_expression2a TIMES inner_expression1a RP {Ttuple($2 @ [$4])}
;

inner_expression1a: 
  type_parser {$1}
;

inner_expression2a:
  inner_expression2a TIMES inner_expression1a {$1 @ [$3]}
  | type_parser {[$1]}
;

const_type:

  TUNIT {Tunit}
| TBOOL {Tbool}
| TINT {Tint}
;

exp_parser:

 bool_expression EOF   { $1 } 
	
;


bool_expression:

     or_expression {$1}
   | bool_expression DISJ or_expression  {Disjunction($1,$3)}	

;

or_expression:

	 and_expression {$1}
    | or_expression CONJ and_expression {Conjunction($1,$3) }
;

and_expression: 

	eq_expression {$1}
  | NOT and_expression {Not($2)}
;


eq_expression: 

 	comp_expression {$1}
 	 | eq_expression EQ comp_expression {Equals($1,$3)}
   | eq_expression GT comp_expression {GreaterT ($1,$3)}
   | eq_expression GT EQ comp_expression {GreaterTE ($1,$4)}
   | eq_expression LT comp_expression {LessT ($1,$3)}
   | eq_expression LT EQ comp_expression {LessTE ($1,$4)}
;

comp_expression: 

    sub_expression {$1}
   | comp_expression MINUS sub_expression {Sub($1,$3)}
   | comp_expression PLUS sub_expression {Add($1,$3)}
;


sub_expression: 

   rem_expression {$1}
  | sub_expression REM rem_expression {Rem($1,$3)}
  | sub_expression TIMES rem_expression {Mult($1,$3)}
  | sub_expression DIV rem_expression {Div($1,$3)}
;

rem_expression:
  
    ifte_expression {$1}
   | ABS rem_expression {Abs($2)}
   | TILDA rem_expression {Negative($2)}
;



ifte_expression: 

     temp_expression {$1}
   
   
   | IF bool_expression THEN bool_expression ELSE bool_expression FI {IfThenElse($2,$4,$6)} 
 ;

temp_expression:

	tuple_expression {$1}
	| PROJ LP INT COMMA INT RP ifte_expression {Project(($3,$5),$7)}

;

tuple_expression:

	paren_expression {$1}
	| LP inner_expression2 COMMA inner_expression1 RP {Tuple(List.length ($2 @ [$4]),($2 @ [$4]))}
	
;

inner_expression2: 

    inner_expression2 COMMA inner_expression1 {($1) @ [($3)]}
    | bool_expression {[$1]}
;

inner_expression1:

		bool_expression {$1}
;

paren_expression:

		func_expression {$1}
  	  | LP bool_expression RP {InParen($2)}
  	  | LET def_expression IN bool_expression END {Let($2,$4)}
  	   
;

func_expression:

	
	 abstr_expression {$1}
	| ifte_expression LP bool_expression RP {FunctionCall($1,$3)}
	
;

abstr_expression:

	const_expression {$1}
	| BACKSLASH ID COLON type_parser DOT const_expression {FunctionAbstraction($2,$4,$6)}
	| BACKSLASH ID COLON type_parser DOT LP bool_expression RP {FunctionAbstraction($2,$4,$7)}

;

const_expression:
  
   ID { Var($1) } 
   | BOOL {B($1)}
   | INT {N($1)}
;	

def_parser:

	def_expression EOF {$1}

;

def_expression:
    basic_expression                          { $1 }
  | def_expression SEMICOLON basic_expression         { match ($1) with
                                            			  Sequence(l) -> Sequence(l @ [$3])
				                                            | _ -> Sequence([$1; $3])
				                                        }

  | def_expression PARALLEL basic_expression          { match ($1) with
                                              				Parallel(l) -> Parallel(l @ [$3])
				                                            | _ -> Parallel([$1;$3])
				                                        }
;

basic_expression:
    DEF ID COLON type_parser EQ bool_expression                     { Simple($2,$4,$6) }
  | LOCAL def_expression IN def_expression END         { Local($2,$4)}
;
