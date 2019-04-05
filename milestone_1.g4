grammar milestone_1;

SPACE: ' ';
NEWLINE: [\r\n]+;

NOT_INDENT: (INDENT (SPACE|SPACE SPACE|SPACE SPACE SPACE) ) -> skip;
INDENT: '    '+ ;

LINE: 
    IMPORT_STMT 
    | TRY_STMT
    | ASM_STMT ;
IMPORT_STMT: 
    IMPORT IDENTIFIER (',' IDENTIFIER)* | IMPORT IDENTIFIER FROM IDENTIFIER ;
COMMENT:
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

HASHTAG: '#' ;
AND: 'and' ;
VARIABLE: 'var' ;
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
IDENTIFIER : LETTER+ ( '_'* (LETTER | DIGIT) )*;
LETTER: [a-zA-Z] ;
DIGIT: [0-9] ;
literal: 
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
ADD_OPERATOR: '+' ;
MUL_OPERATOR: '*' ;
MINUS_OPERATOR: '-' ;
DIV_OPERATOR: '/' ;
BITWISE_NOT_OPERATOR: '!' ;
AND_OPERATOR: '&' ;
OR_OPERATOR: '|' ;
LESS_THAN: '<' ;
GREATER_THAN: '>' ;
AT: '@' ;
MODULUS: '%';
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
CHAR_LIT: '\'\\t\'' | '\'\\r\'' | '\'\\c\'' | '\'\\n\'' | '\'\\l\'' | '\'\\f\''  | '\'\\v\'' | '\'\\\\\'' | '\'\\\"\'' | '\'\\\'\'' | '\'\\a\'' | '\'\\b\'' | '\'\\e\'' | '\'\\x\'' | ('\'' [0-9a-zA-Z] '\'') ;
STR_LIT: '"' ( ~["\n\r\u2028\u2029])* '"' ;
TRIPLESTR_LIT:'"""' ANY* '"""';
RSTR_LIT: 'r' '"' ( '\\' [btnfr"'\\] | ~[\r\n\\"] )* '"';
GENERALIZED_STR_LIT: IDENTIFIER STR_LIT;
GENERALIZED_TRIPLESTR_LIT: IDENTIFIER TRIPLESTR_LIT;


ANY: .;