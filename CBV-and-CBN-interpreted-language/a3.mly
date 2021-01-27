%{
    open A1
%}

/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token PLUS TIMES MINUS GT CONJ DISJ LP RP IF THEN ELSE FI BACKSLASH DOT CMP TUNIT TBOOL TINT COLON EOF
%start type_parser exp_parser
%type <A1.exptree> exp_parser /* Returns expression */
%type <A1.exptype> type_parser /* Returns expression type */
%%

type_parser:
   const_type {$1}
 | type_parser MINUS GT const_type {Tfunc($1, $4)}
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
   | bool_expression DISJ or_expression  {Or($1,$3)}	

;

or_expression:

	 eq_expression {$1}
    | or_expression CONJ eq_expression {And($1,$3) }
;


eq_expression: 

 	comp_expression {$1}
 	 | CMP LP comp_expression RP {Cmp($3)}
;

comp_expression: 

    sub_expression {$1}
   | comp_expression PLUS sub_expression {Plus($1,$3)}
;


sub_expression: 

   rem_expression {$1}
  | sub_expression TIMES rem_expression {Mult($1,$3)}
;


rem_expression: 

     temp_expression {$1} 
   | IF bool_expression THEN bool_expression ELSE bool_expression FI {If_Then_Else($2,$4,$6)} 
 ;


temp_expression:

	
	 abstr_expression {$1}
	| rem_expression LP bool_expression RP {App($1,$3)}
	
;

abstr_expression:

	const_expression {$1}
	| BACKSLASH const_expression COLON type_parser DOT const_expression {Lambda($2,$4,$6)}
	| BACKSLASH const_expression COLON type_parser DOT LP bool_expression RP {Lambda($2,$4,$7)}

;

const_expression:
  
   ID { V($1) } 
   | BOOL {Bool($1)}
   | INT {Integer($1)}
;	


