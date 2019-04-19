grammar milestone_2;
//antlr milestone_2.g4 -Dlanguage=Python3

identifier : IDENTIFIER;
int_lit: INT_LIT;
variable: unary_operation* ( identifier | ((identifier|'`$`') OPEN_BRACK (variable|int_lit|operation_expr) CLOSE_BRACK));

operator: MUL_OPERATOR | ADD_OPERATOR | MINUS_OPERATOR | AND_OPERATOR | DIV_OPERATOR | DIV | MOD;
unary_operation: '$';
var_type: INT|STRING|BOOL|UNTYPED|identifier|(var? identifier (OPEN_BRACK (var_type) CLOSE_BRACK)?);
STRING: 'string';
INT: 'int';
BOOL: 'bool';
UNTYPED: 'untyped';
colon: COLON;
comma: COMMA;
var: VARIABLE;
equal: EQUAL;
equal_equal: '==';
if_kw: 'if';
elif_kw: 'elif';
else_kw: 'else';
echo: 'echo';

init_block: 
        var (identifier (comma identifier)* colon var_type? NEWLINE* )+ (COMMENT)* NEWLINE* (assign_expr NEWLINE*)*
        | const assign_expr? (assign_expr)*
        | (let|var) (assign_expr)+;

echo_stmt: echo OPEN_PAREN? (literal| variable|invoke_func) (comma (literal|variable|invoke_func|assign_expr|(OPEN_PAREN? conditional_assign_if_stmt CLOSE_PAREN?)))* CLOSE_PAREN?;

assign_expr: var? variable equal ('@'? array|(operator? literal)|variable|operation_expr|true|false|invoke_func) SEMI_COLON?;

conditional_assign: var identifier equal conditional_assign_if_stmt;
conditional_assign_if_stmt: if_kw condition colon (literal|variable|invoke_func|operation_expr) else_kw colon (literal|variable|invoke_func);

operation_expr:OPEN_PAREN? (variable|literal|invoke_func) (operator (variable|literal|invoke_func))+ CLOSE_PAREN?;
operation_expr_dash: operation_expr (operator (operation_expr))+ ;

case_block: case identifier
            (of literal (comma literal)* colon
            (stmt)+)+
            (else_kw colon discard?)?;

return_stmt: return_kw (operation_expr|literal|true|false)?;

assert_stmt: assert_kw condition;

assert_kw: 'assert';

if_stmt: if_kw condition ((AND|OR)condition)* colon
        (stmt)+
        (elif_kw condition ((AND|OR)condition)* colon
        (stmt)+)*
        (else_kw colon )?;

condition: not_kw?(
    (variable|operation_expr|invoke_func|literal) 
    ((equal_equal | less_than |less_than_equal|greater_than|greater_than_equal) (variable|literal|invoke_func))?
    | invoke_func
    |true|false) ;
          
loop: for_loop  
    | while_loop;

//proc_call: variable (OPEN_BRACK var_type CLOSE_BRACK)? OPEN_PAREN? 
//        ((variable|literal|operation_expr_dash|operation_expr|proc_call) (comma(variable|literal|operation_expr|proc_call))*)? CLOSE_PAREN?;

type_call: variable OPEN_PAREN (identifier colon (literal|true|false|variable)) 
        (comma identifier colon (literal|true|false|variable))* CLOSE_PAREN;

invoke_func: (( variable|literal) DOT (identifier))
            | ((identifier|literal) DOT (identifier) OPEN_PAREN? (invoke_func|variable|literal) CLOSE_PAREN?)
            | (OPEN_PAREN operation_expr CLOSE_PAREN DOT identifier)
            | ((variable) (OPEN_BRACK var_type CLOSE_BRACK)? OPEN_PAREN? ((variable|literal|operation_expr_dash|operation_expr|invoke_func) 
                (comma (variable|literal|operation_expr_dash|operation_expr|invoke_func))*)? CLOSE_PAREN?);
            
//add_func: 'add';
//len_func: 'len';
//high_func: 'high';

for_loop: for_kw identifier (comma identifier)* in_kw '@'? (array|variable) colon
        (stmt|continue_kw)+ 
        (break_kw)?;

while_loop: while_kw condition ((AND|OR)condition)* colon
        (stmt|continue_kw)+
        (break_kw)?;

when_stmt: when condition ((AND|OR)condition)* colon
            (stmt)+
            (elif_kw condition ((AND|OR)condition)* colon
            (stmt)+)*
            (else_kw colon )?;

array: 'items' OPEN_PAREN identifier CLOSE_PAREN
        | 'countdown' OPEN_PAREN literal comma literal CLOSE_PAREN
        | OPEN_BRACK operator? literal? (comma operator? literal)* CLOSE_BRACK
        | INT_LIT DOT DOT less_than invoke_func
        | literal DOT DOT literal 
        | invoke_func
        | (OPEN_BRACK invoke_func CLOSE_BRACK);

proc_block: proc variable OPEN_PAREN identifier colon (var_type|proc_type|('varargs' OPEN_BRACK variable CLOSE_BRACK))
            (comma (assign_expr|(identifier colon (var_type|proc_type))))*  CLOSE_PAREN (colon (var_type|variable))? equal
            stmt+
            condition?;

proc_type: proc OPEN_PAREN identifier colon var_type CLOSE_PAREN;

block_block: block identifier colon
            (stmt)+
           ( break_kw identifier)?;

type_block: type_kw 
            (assign_type)+
            (case_type)?;

case_type: case identifier colon var_type
            (of (literal|true|false) (comma literal)* colon
            variable colon variable)+
            ;

assign_type: variable equal (type_array| (ref variable) |object_kw);

type_array: 'array' OPEN_BRACK ((literal DOT DOT literal)|int_lit) comma var_type CLOSE_BRACK;

import_stmt: import_kw identifier (comma identifier)*;

from_stmt: from_kw identifier import_stmt;

macro_block: macro identifier OPEN_PAREN identifier colon var_type (SEMI_COLON identifier colon var_type)* CLOSE_PAREN colon var_type equal
            template_block+;

template_block: template identifier OPEN_PAREN identifier (comma identifier)* CLOSE_PAREN (OPEN_BRACE DOT identifier CLOSE_BRACE)? equal
                (stmt|identifier)+;

break_stmt: break_kw identifier;

SPACE: ' ' -> skip;
NEWLINE: ('\n'|'\r' )+ ->skip;


COMMENT: NEWLINE (MULTI_LINE_COMMENT | SINGLE_LINE_COMMENT | IND_COMMENT) -> skip;
IND_COMMENT: INDENT+ (SINGLE_LINE_COMMENT|MULTI_LINE_COMMENT);
SINGLE_LINE_COMMENT:
    '#' ~('\r' | '\n' | '#')* -> skip ;
MULTI_LINE_COMMENT:
    INDENT* (HASHTAG OPEN_BRACK (ANY)* CLOSE_BRACK HASHTAG 
    |HASHTAG HASHTAG OPEN_BRACK (ANY)* CLOSE_BRACK HASHTAG HASHTAG) -> skip ;

NOT_INDENT: (INDENT (SPACE|SPACE SPACE|SPACE SPACE SPACE) ) -> skip;
INDENT: '    '->skip;
HASHTAG: '#' ;
AND: 'and' ;
VARIABLE: 'var' ;
ADDR: 'addr';
AS: 'as' ;
ASM: 'asm' ;
BIND: 'bind' ;
block: 'block' ;
break_kw: 'break' ;
case: 'case' ;
CAST: 'cast' ;
CONCEPT: 'concept' ;
const: 'const' ;
continue_kw: 'continue' ;
CONVERTER: 'converter' ;
DEFER: 'defer' ;
discard: 'discard' ;
DISTINCT: 'distinct' ;
DIV: 'div' ;
DO: 'do' ;
//ELIF: 'elif' ;
//ELSE: 'else' ;
END: 'end' ;
ENUM: 'enum' ;
EXCEPT: 'except' ;
EXPORT: 'export' ;
FINALLY: 'finally' ;
for_kw: 'for' ;
from_kw: 'from' ;
FUNC: 'func' ;
//IF: 'if' ;
import_kw: 'import' ;
in_kw: 'in' ;
INCLUDE: 'include' ;
INTERFACE: 'interface' ;
IS: 'is' ;
ISNOT: 'isnot' ;
ITERATOR: 'iterator' ;
let: 'let' ;
macro: 'macro' ;
METHOD: 'method' ;
MIXIN: 'mixin' ;
MOD: 'mod' ;
NIL: 'nil' ;
not_kw: 'not' ;
NOTIN: 'notin' ;
object_kw: 'object' ;
of: 'of' ;
OR: 'or' ;
OUT: 'out' ;
proc: 'proc' ;
PTR: 'ptr' ;
RAISE: 'raise' ;
ref: 'ref' ;
return_kw: 'return' ;
SHL: 'shl' ;
SHR: 'shr' ;
STATIC: 'static' ;
template: 'template' ;
TRY: 'try' ;
TUPLE: 'tuple' ;
type_kw: 'type' ;
USING: 'using' ;
when: 'when' ;
while_kw: 'while' ;
XOR: 'xor' ;
YIELD: 'yield' ;
UNDERSCORE: '_';
true: 'true';
false: 'false';
IDENTIFIER : [a-zA-Z_][a-zA-Z_0-9]* ;
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
EQUAL: '=';
EQUALS_OPERATOR: '=''='? ;
ADD_OPERATOR: '+' ;
MUL_OPERATOR: '*' ;
MINUS_OPERATOR: '-' ;
DIV_OPERATOR: '/' ;
BITWISE_NOT_OPERATOR: '~' ;
AND_OPERATOR: '&' ;
OR_OPERATOR: '|' ;
less_than: '<' ;
greater_than: '>' ;
less_than_equal: '<=' ;
greater_than_equal: '>=' ;
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
CHAR_LIT: '\'\\t\'' | '\'\\r\'' | '\'\\c\'' | '\'\\n\'' | '\'\\l\'' | '\'\\f\''  | '\'\\v\'' | '\'\\\\\'' | '\'\\"\'' 
        | '\'\\\'\'' | '\'\\a\'' | '\'\\b\'' | '\'\\e\'' | '\'\\x\'' | ('\'' [0-9a-zA-Z] '\'') | '\'[\'' | '\']\'' ;
STR_LIT: '"' ( ~["\n\r\u2028\u2029])* '"' ;
TRIPLESTR_LIT:'"""' ANY* '"""';
RSTR_LIT: 'r' '"' ( '\\' [btnfr"'\\] | ~[\r\n\\"] )* '"';
GENERALIZED_STR_LIT: IDENTIFIER STR_LIT;
GENERALIZED_TRIPLESTR_LIT: IDENTIFIER TRIPLESTR_LIT;
DIGIT: [0-9] ;
LETTER: [a-zA-Z] ;
SINGLE_QUOTE: '\'';

ANY: . -> skip;

stmt:(init_block | assign_expr | echo_stmt | if_stmt | case_block | loop | when_stmt | proc_block | from_stmt 
    | type_call | return_stmt | block_block | type_block | invoke_func | conditional_assign | import_stmt 
    | assert_stmt | macro_block | break_stmt ) NEWLINE*;

start: (stmt)* EOF?;