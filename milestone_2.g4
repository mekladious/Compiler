grammar milestone_2;

SPACE: ' ' -> skip;
NEWLINE: [\r\n]+ -> skip;

NOT_INDENT: (INDENT (SPACE|SPACE SPACE|SPACE SPACE SPACE) ) -> skip;
INDENT: '    '+ ;

OPERATOR :  OP2 | OP3 | OP4 | OP5 | OP6 | OP7 | OP8 | OP9 | OP10
        | OR | XOR | AND | IS | ISNOT | IN | NOTIN | OF | DIV | MOD | SHL | SHR | NOT | STATIC ;

OP2 : AT | COLON ;
OP3 : OR | XOR ;
OP4 : AND ;
OP5 : EQUALS_OPERATOR | LE_OPERATOR | LESS_THAN | GE_OPERATOR | GREATER_THAN | IN | NOTIN | IS 
    | ISNOT | NOT | OF ;
OP6 : '..';
OP7 : AND_OPERATOR ;
OP8 : ADD_OPERATOR | MINUS_OPERATOR ;
OP9 : MUL_OPERATOR | DIV_OPERATOR | DIV | MOD | SHL | SHR | MODULUS ;
OP10 : DOLLAR | XOR_OPERATOR;

OPT_IND : COMMENT? INDENT?;
OPT_PAR : (INDENT{>} | INDENT{=})?;

ASSIGN_EXPR : OR_EXPR (OP2 OPT_IND OR_EXPR)* ;
OR_EXPR : AND_EXPR (OP3 OPT_IND AND_EXPR)* ;
AND_EXPR : CMP_EXPR (OP4 OPT_IND CMP_EXPR)* ;
CMP_EXPR : SLICE_EXPR (OP5 OPT_IND SLICE_EXPR)* ;
SLICE_EXPR : AMP_EXPR (OP6 OPT_IND AMP_EXPR)* ;
AMP_EXPR : PLUS_EXPR (OP7 OPT_IND PLUS_EXPR)* ;
PLUS_EXPR : MUL_EXPR (OP8 OPT_IND MUL_EXPR)* ;
MUL_EXPR : DOLLAR_EXPR (OP9 OPT_IND DOLLAR_EXPR)* ;
DOLLAR_EXPR : PRIMARY (OP10 OPT_IND PRIMARY)* ;

PRIMARY :
          OPERATOR* INDENT_LIT 
        | 'bind' PRIMARY;

EXPR_COLON_EXPR : EXPR (':'|'=' EXPR)?;

EXPR : (BLOCK_EXPR
      | IF_EXPR
      | WHEN_EXPR
      | FOR_STMT
      | TRY_EXPR);


COLCOM : COLON COMMENT?;
IF_EXPR: IF COND_EXPR;
COND_EXPR: EXPR COLCOM EXPR OPT_IND
        (ELIF EXPR COLCOM EXPR OPT_IND)*
         ELSE COLCOM EXPR;
WHEN_EXPR: WHEN COND_EXPR;
BLOCK_EXPR : BLOCK SYMBOL? COLCOM STMT;
FOR_STMT: FOR (IDENT_PRAGMA ^+ COMMA) IN EXPR C stmt;

PRAGMA : OPEN_BRACE DOT OPT_IND (EXPR_COLON_EXPR COMMA?)* OPT_PAR (DOT CLOSE_BRACE | CLOSE_BRACE);
IDENT_VIS = SYMBOL OPERATOR?  # postfix position
IDENT_PRAGMA = identVis PRAGMA?

SYMBOL : '`' (KEYW|IDENTIFIER|LITERAL|(OPERATOR|'('|')'|'['|']'|'{'|'}'|'=')+)+ '`' | IDENTIFIER | KEYW ;
STMT: 
    IMPORT_STMT 
    | TRY_STMT
    | ASM_STMT 
    | CASE_STMT;

CASE_STMT : CASE EXPR ':'? COMMENT?;
IMPORT_STMT: 
    IMPORT IDENTIFIER (',' IDENTIFIER)* | IMPORT IDENTIFIER FROM IDENTIFIER ;
COMMENT: MULTI_LINE_COMMENT | SINGLE_LINE_COMMENT ;
SINGLE_LINE_COMMENT:
    '#' ~('\r' | '\n' | '#')* -> skip ;
MULTI_LINE_COMMENT:
    INDENT* (HASHTAG OPEN_BRACK (ANY)* CLOSE_BRACK HASHTAG 
    |HASHTAG HASHTAG OPEN_BRACK (ANY)* CLOSE_BRACK HASHTAG HASHTAG) -> skip ;
ADDR_OPER: 
    ADDR OPEN_PAREN IDENTIFIER CLOSE_PAREN ;
TRY_STMT:
    TRY COLON ;
ASM_STMT:
    ASM (STR_LIT | RSTR_LIT | TRIPLESTR_LIT) ;
BIND_STMT:
    BIND;
CAST_EXPR: 
    CAST;

GEN_LIT : GENERALIZED_STR_LIT | GENERALIZED_TRIPLESTR_LIT ;
INDENT_LIT : GEN_LIT | SYMBOL | LITERAL
            | par | arrayConstr | setOrTableConstr
            | CAST_EXPR;

TYPE_KEYW : VARIABLE | OUT | REF | PTR | SHARED | TUPLE | PROC | ITERATOR | DISTINCT | OBJECT | ENUM ;
PAR_KEYW : DISCARD | INCLUDE | IF | WHILE | CASE | TRY | FINALLY | EXCEPT | FOR | BLOCK 
        | CONST | LET | WHEN | VARIABLE | MIXIN ;

SHARED : 'shared' ;
HASHTAG: '#' ;
AND: 'and' ;
VARIABLE: 'var' ;
KEYW : ADDR | AS | ASM | BIND | BLOCK | BREAK | CASE | CAST | CONCEPT | CONST | CONTINUE | CONVERTER
    | DEFER | DISCARD | DISTINCT | DIV | DO | ELIF | ELSE | END | ENUM | EXCEPT | EXPORT | FINALLY
    | FOR | FROM | FUNC | IF | IMPORT | IN | INCLUDE | INTERFACE | IS | ISNOT | ITERATOR | LET 
    | MACRO | METHOD | MIXIN | MOD | NIL | NOTIN | OBJECT | OF | OR | OUT | PROC | PTR | RAISE | REF
    | RETURN | SHL | SHR | STATIC | TEMPLATE | TRY | TUPLE | TYPE | USING | VARIABLE | WHEN | WHILE
    | XOR | YIELD ;

ADDR: 'addr';
AS: 'as' ;
ASM: 'asm' ;
BIND: 'bind' ;
BLOCK: 'block' ;
BREAK: 'break' ;
CASE: 'case' ;
CAST: 'cast' ;
CONCEPT: 'concept' ;
CONST: 'const' ;
CONTINUE: 'continue' ;
CONVERTER: 'converter' ;
DEFER: 'defer' ;
DISCARD: 'discard' ;
DISTINCT: 'distinct' ;
DIV: 'div' ;
DO: 'do' ;
ELIF: 'elif' ;
ELSE: 'else' ;
END: 'end' ;
ENUM: 'enum' ;
EXCEPT: 'except' ;
EXPORT: 'export' ;
FINALLY: 'finally' ;
FOR: 'for' ;
FROM: 'from' ;
FUNC: 'func' ;
IF: 'if' ;
IMPORT: 'import' ;
IN: 'in' ;
INCLUDE: 'include' ;
INTERFACE: 'interface' ;
IS: 'is' ;
ISNOT: 'isnot' ;
ITERATOR: 'iterator' ;
LET: 'let' ;
MACRO: 'macro' ;
METHOD: 'method' ;
MIXIN: 'mixin' ;
MOD: 'mod' ;
NIL: 'nil' ;
NOT: 'not' ;
NOTIN: 'notin' ;
OBJECT: 'object' ;
OF: 'of' ;
OR: 'or' ;
OUT: 'out' ;
PROC: 'proc' ;
PTR: 'ptr' ;
RAISE: 'raise' ;
REF: 'ref' ;
RETURN: 'return' ;
SHL: 'shl' ;
SHR: 'shr' ;
STATIC: 'static' ;
TEMPLATE: 'template' ;
TRY: 'try' ;
TUPLE: 'tuple' ;
TYPE: 'type' ;
USING: 'using' ;
WHEN: 'when' ;
WHILE: 'while' ;
XOR: 'xor' ;
YIELD: 'yield' ;
IDENTIFIER : LETTER+ ( '_'?(LETTER | DIGIT) )*;
LETTER: [a-zA-Z] ;
DIGIT: [0-9] ;
LITERAL: 
    INT_LIT | INT8_LIT | INT16_LIT | INT32_LIT | INT64_LIT
    | UINT_LIT | UINT8_LIT | UINT16_LIT | UINT32_LIT | UINT64_LIT
    | FLOAT_LIT | FLOAT32_LIT | FLOAT64_LIT
    | STR_LIT | RSTR_LIT | TRIPLESTR_LIT
    | CHAR_LIT
    | NIL ;
INT_LIT:
         HEX_LIT
        | DEC_LIT
        | OCT_LIT
        | BIN_LIT ;
HEX_LIT : '0' ('x' | 'X' ) HEXDIGIT+ ( '_' HEXDIGIT )* ;
DEC_LIT : DIGIT+ ( '_' DIGIT+ )* ;
OCT_LIT : '0' 'o' OCTDIGIT+ ( '_' OCTDIGIT )* ;
BIN_LIT : '0' ('b' | 'B' ) BINDIGIT+ ( '_' BINDIGIT+ )*  ;
INT8_LIT: INT_LIT '\'' ('i' | 'I') '8' ;
INT16_LIT: INT_LIT '\'' ('i' | 'I') '16' ;
INT32_LIT: INT_LIT '\'' ('i' | 'I') '32' ;
INT64_LIT: INT_LIT '\'' ('i' | 'I') '64' ;
UINT_LIT: INT_LIT '\'' ('u' | 'U') ;
UINT8_LIT: UINT_LIT '8' ;
UINT16_LIT: UINT_LIT '16' ;
UINT32_LIT: UINT_LIT '32' ;
UINT64_LIT: UINT_LIT '64' ;
FLOAT_LIT: DIGIT+ ('_' DIGIT)* (('.' DIGIT+ ('_' DIGIT)* EXP?) |EXP);
FLOAT32_LIT: HEX_LIT '\'' FLOAT32_SUFFIX
            | (FLOAT_LIT | DEC_LIT | OCT_LIT | BIN_LIT) '\'' FLOAT32_SUFFIX ;
FLOAT32_SUFFIX:  ('f' | 'F') '32' ;
FLOAT64_LIT: HEX_LIT '\'' FLOAT64_SUFFIX
            | (FLOAT_LIT | DEC_LIT | OCT_LIT | BIN_LIT) '\'' FLOAT64_SUFFIX ;
FLOAT64_SUFFIX: ( ('f' | 'F') '64' ) | 'd' | 'D' ; 
EXP: ('e' | 'E' ) [+-] DIGIT+ ( [_] DIGIT )* ;
HEXDIGIT: DIGIT | [A-Fa-f] ;
OCTDIGIT: [0-7] ;
BINDIGIT: [0-1] ;
EQUALS_OPERATOR: '=''='? ;
LE_OPERATOR: LESS_THAN '=' ;
GE_OPERATOR: GREATER_THAN '=' ;
ADD_OPERATOR: '+' ;
MUL_OPERATOR: '*' ;
MINUS_OPERATOR: '-' ;
DIV_OPERATOR: '/' ;
BITWISE_NOT_OPERATOR: '~' ;
AND_OPERATOR: '&' ;
OR_OPERATOR: '|' ;
LESS_THAN: '<' ;
GREATER_THAN: '>' ;
AT: '@' ;
MODULUS: '%';
NOT_OPERATOR: '!';
XOR_OPERATOR: '^';
DOT: '.' ;
COLON: ':' ;
OPEN_PAREN: '(' ;
CLOSE_PAREN: ')' ;
OPEN_BRACE: '{' ;
CLOSE_BRACE: '}' ;
OPEN_BRACK: '[' ;
CLOSE_BRACK: ']' ;
COMMA: ',' ;
SEMI_COLON: ';' ;
DOLLAR : '$' ;
CHAR_LIT: '\'\\t\'' | '\'\\r\'' | '\'\\c\'' | '\'\\n\'' | '\'\\l\'' | '\'\\f\''  | '\'\\v\'' | '\'\\\\\'' | '\'\\"\'' | '\'\\\'\'' | '\'\\a\'' | '\'\\b\'' | '\'\\e\'' | '\'\\x\'' | ('\'' [0-9a-zA-Z] '\'') ;
STR_LIT: '"' ( ~["\n\r\u2028\u2029])* '"' ;
TRIPLESTR_LIT:'"""' ANY* '"""';
RSTR_LIT: 'r' '"' ( '\\' [btnfr"'\\] | ~[\r\n\\"] )* '"';
GENERALIZED_STR_LIT: IDENTIFIER STR_LIT;
GENERALIZED_TRIPLESTR_LIT: IDENTIFIER TRIPLESTR_LIT;

ANY: . -> skip;