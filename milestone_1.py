# antlr milestone_1.g4 -Dlanguage=Python3
import argparse
from antlr4 import *
from milestone_1Lexer import milestone_1Lexer
from milestone_1Listener import milestone_1Listener
from milestone_1Parser import milestone_1Parser
from antlr4.tree.Trees import Trees

def get_token_type(token):
    print(token.type)
    if token.type == milestone_1Lexer.AND:
        return "AND"
    elif token.type == milestone_1Lexer.INDENT:
        return "INDENT"
    elif token.type == milestone_1Lexer.VARIABLE:
        return "VARIABLE"
    elif token.type == milestone_1Lexer.ADDR:
        return "ADDR"
    elif token.type == milestone_1Lexer.AS:
        return "AS"
    elif token.type == milestone_1Lexer.ASM:
        return "ASM"
    elif token.type == milestone_1Lexer.BIND:
        return "BIND"
    elif token.type == milestone_1Lexer.BLOCK:
        return "BLOCK"
    elif token.type == milestone_1Lexer.BREAK:
        return "BREAK"
    elif token.type == milestone_1Lexer.CASE:
        return "CASE"
    elif token.type == milestone_1Lexer.CAST:
        return "CAST"
    elif token.type == milestone_1Lexer.CONCEPT:
        return "CONCEPT"
    elif token.type == milestone_1Lexer.CONST:
        return "CONST"
    elif token.type == milestone_1Lexer.CONTINUE:
        return "CONTINUE"
    elif token.type == milestone_1Lexer.CONVERTER:
        return "CONVERTER"
    elif token.type == milestone_1Lexer.DEFER:
        return "DEFER"
    elif token.type == milestone_1Lexer.DISCARD:
        return "DISCARD"
    elif token.type == milestone_1Lexer.DISTINCT:
        return "DISTINCT"
    elif token.type == milestone_1Lexer.DIV:
        return "DIV"
    elif token.type == milestone_1Lexer.DO:
        return "DO"
    elif token.type == milestone_1Lexer.ELIF:
        return "ELIF"
    elif token.type == milestone_1Lexer.ELSE:
        return "ELSE"
    elif token.type == milestone_1Lexer.END:
        return "END"
    elif token.type == milestone_1Lexer.ENUM:
        return "ENUM"
    elif token.type == milestone_1Lexer.EXCEPT:
        return "EXCEPT"
    elif token.type == milestone_1Lexer.EXPORT:
        return "EXPORT"
    elif token.type == milestone_1Lexer.FINALLY:
        return "FINALLY"
    elif token.type == milestone_1Lexer.FOR:
        return "FOR"
    elif token.type == milestone_1Lexer.FROM:
        return "FROM"
    elif token.type == milestone_1Lexer.FUNC:
        return "FUNC"
    elif token.type == milestone_1Lexer.IF:
        return "IF"
    elif token.type == milestone_1Lexer.IMPORT:
        return "IMPORT"
    elif token.type == milestone_1Lexer.IN:
        return "IN"
    elif token.type == milestone_1Lexer.INCLUDE:
        return "INCLUDE"
    elif token.type == milestone_1Lexer.INTERFACE:
        return "INTERFACE"
    elif token.type == milestone_1Lexer.IS:
        return "IS"
    elif token.type == milestone_1Lexer.ISNOT:
        return "ISNOT"
    elif token.type == milestone_1Lexer.ITERATOR:
        return "ITERATOR"
    elif token.type == milestone_1Lexer.LET:
        return "LET"
    elif token.type == milestone_1Lexer.MACRO:
        return "MACRO"
    elif token.type == milestone_1Lexer.METHOD:
        return "METHOD"
    elif token.type == milestone_1Lexer.MIXIN:
        return "MIXIN"
    elif token.type == milestone_1Lexer.MOD:
        return "MOD"
    elif token.type == milestone_1Lexer.NIL:
        return "NIL"
    elif token.type == milestone_1Lexer.NOT:
        return "NOT"
    elif token.type == milestone_1Lexer.NOTIN:
        return "NOTIN"
    elif token.type == milestone_1Lexer.OBJECT:
        return "OBJECT"
    elif token.type == milestone_1Lexer.OF:
        return "OF"
    elif token.type == milestone_1Lexer.OR:
        return "OR"
    elif token.type == milestone_1Lexer.OUT:
        return "OUT"
    elif token.type == milestone_1Lexer.PROC:
        return "PROC"
    elif token.type == milestone_1Lexer.PTR:
        return "PTR"
    elif token.type == milestone_1Lexer.RAISE:
        return "RAISE"
    elif token.type == milestone_1Lexer.REF:
        return "REF"
    elif token.type == milestone_1Lexer.RETURN:
        return "RETURN"
    elif token.type == milestone_1Lexer.SHL:
        return "SHL"
    elif token.type == milestone_1Lexer.SHR:
        return "SHR"
    elif token.type == milestone_1Lexer.STATIC:
        return "STATIC"
    elif token.type == milestone_1Lexer.TEMPLATE:
        return "TEMPLATE"
    elif token.type == milestone_1Lexer.TRY:
        return "TRY"
    elif token.type == milestone_1Lexer.TUPLE:
        return "TUPLE"
    elif token.type == milestone_1Lexer.TYPE:
        return "TYPE"
    elif token.type == milestone_1Lexer.USING:
        return "USING"
    elif token.type == milestone_1Lexer.WHEN:
        return "WHEN"
    elif token.type == milestone_1Lexer.WHILE:
        return "WHILE"
    elif token.type == milestone_1Lexer.XOR:
        return "XOR"
    elif token.type == milestone_1Lexer.YIELD:
        return "YIELD"
    elif token.type == milestone_1Lexer.IDENTIFIER:
        return "IDENTIFIER"
    elif token.type == milestone_1Lexer.LETTER:
        return "LETTER"
    elif token.type == milestone_1Lexer.DIGIT:
        return "DIGIT"
    elif token.type == milestone_1Lexer.INT8_LIT:
        return "INT8_LIT"
    elif token.type == milestone_1Lexer.INT16_LIT:
        return "INT16_LIT"
    elif token.type == milestone_1Lexer.INT32_LIT:
        return "INT32_LIT"
    elif token.type == milestone_1Lexer.INT64_LIT:
        return "INT64_LIT"
    elif token.type == milestone_1Lexer.UINT_LIT:
        return "UINT_LIT"
    elif token.type == milestone_1Lexer.UINT8_LIT:
        return "UINT8_LIT"
    elif token.type == milestone_1Lexer.UINT16_LIT:
        return "UINT16_LIT"
    elif token.type == milestone_1Lexer.UINT32_LIT:
        return "UINT32_LIT"
    elif token.type == milestone_1Lexer.UINT64_LIT:
        return "UINT64_LIT"
    elif token.type == milestone_1Lexer.FLOAT32_LIT:
        return "FLOAT32_LIT"
    elif token.type == milestone_1Lexer.FLOAT32_SUFFIX:
        return "FLOAT32_SUFFIX"
    elif token.type == milestone_1Lexer.FLOAT64_LIT:
        return "FLOAT64_LIT"
    elif token.type == milestone_1Lexer.FLOAT64_SUFFIX:
        return "FLOAT64_SUFFIX"
    elif token.type == milestone_1Lexer.FLOAT_LIT:
        return "FLOAT_LIT"
    elif token.type == milestone_1Lexer.EXP:
        return "EXP"
    elif token.type == milestone_1Lexer.INT_LIT:
        return "INT_LIT"
    elif token.type == milestone_1Lexer.HEX_LIT:
        return "HEX_LIT"
    elif token.type == milestone_1Lexer.DEC_LIT:
        return "DEC_LIT"
    elif token.type == milestone_1Lexer.OCT_LIT:
        return "OCT_LIT"
    elif token.type == milestone_1Lexer.BIN_LIT:
        return "BIN_LIT"
    elif token.type == milestone_1Lexer.HEXDIGIT:
        return "HEXDIGIT"
    elif token.type == milestone_1Lexer.OCTDIGIT:
        return "OCTDIGIT"
    elif token.type == milestone_1Lexer.BINDIGIT:
        return "BINDIGIT"
    elif token.type == milestone_1Lexer.EQUALS_OPERATOR:
        return "EQUALS_OPERATOR"
    elif token.type == milestone_1Lexer.ADD_OPERATOR:
        return "ADD_OPERATOR"
    elif token.type == milestone_1Lexer.MUL_OPERATOR:
        return "MUL_OPERATOR"
    elif token.type == milestone_1Lexer.MINUS_OPERATOR:
        return "MINUS_OPERATOR"
    elif token.type == milestone_1Lexer.DIV_OPERATOR:
        return "DIV_OPERATOR"
    elif token.type == milestone_1Lexer.BITWISE_NOT_OPERATOR:
        return "BITWISE_NOT_OPERATOR"
    elif token.type == milestone_1Lexer.AND_OPERATOR:
        return "AND_OPERATOR"
    elif token.type == milestone_1Lexer.OR_OPERATOR:
        return "OR_OPERATOR"
    elif token.type == milestone_1Lexer.LESS_THAN:
        return "LESS_THAN"
    elif token.type == milestone_1Lexer.GREATER_THAN:
        return "GREATER_THAN"
    elif token.type == milestone_1Lexer.AT:
        return "AT"
    elif token.type == milestone_1Lexer.MODULUS:
        return "MODULUS"
    # elif token.type == milestone_1Lexer.NOT_OPERATOR:
    #     return "NOT_OPERATOR"
    # elif token.type == milestone_1Lexer.XOR_OPERATOR:
    #     return "XOR_OPERATOR"
    elif token.type == milestone_1Lexer.DOT:
        return "DOT"
    elif token.type == milestone_1Lexer.COLON:
        return "COLON"
    elif token.type == milestone_1Lexer.OPEN_PAREN:
        return "OPEN_PAREN"
    elif token.type == milestone_1Lexer.CLOSE_PAREN:
        return "CLOSE_PAREN"
    elif token.type == milestone_1Lexer.OPEN_BRACE:
        return "OPEN_BRACE"
    elif token.type == milestone_1Lexer.CLOSE_BRACE:
        return "CLOSE_BRACE"
    elif token.type == milestone_1Lexer.OPEN_BRACK:
        return "OPEN_BRACK"
    elif token.type == milestone_1Lexer.CLOSE_BRACK:
        return "CLOSE_BRACK"
    elif token.type == milestone_1Lexer.COMMA:
        return "COMMA"
    elif token.type == milestone_1Lexer.SEMI_COLON:
        return "SEMI_COLON"
    elif token.type == milestone_1Lexer.STR_LIT:
        return "STR_LIT"
    elif token.type == milestone_1Lexer.CHAR_LIT:
        return "CHAR_LIT"
    elif token.type == milestone_1Lexer.TRIPLESTR_LIT:
        return "TRIPLESTR_LIT"
    # elif token.type == milestone_1Lexer.TRIPLESTR_ITEM:
    #     return "TRIPLESTR_ITEM"
    elif token.type == milestone_1Lexer.RSTR_LIT:
        return "RSTR_LIT"
    elif token.type == milestone_1Lexer.MUL_OPERATOR:
        return "GENERALIZED_STR_LIT"
    elif token.type == milestone_1Lexer.MUL_OPERATOR:
        return "GENERALIZED_TRIPLESTR_LIT"
    return "NONE"

def main():
    with open(args.file, "r") as file:
        lines = file.read()
    input_stream = InputStream(lines)
    lexer = milestone_1Lexer(input_stream)
    # token_stream = CommonTokenStream(lexer)
    # parser = milestone_1Parser(token_stream)

    # tree = parser.start()
    # print(Trees.toStringTree(tree,None, parser))

    token = lexer.nextToken()

    output_file = open("milestone_1_result.txt", "w+")
    while not token.type == Token.EOF:
        if get_token_type(token)!="NONE":
            output_file.write(get_token_type(token) + ' ' +  token.text + '\n')
            print(get_token_type(token), token.text)
        token = lexer.nextToken()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(add_help=True, description='Sample Commandline')

    parser.add_argument('--file', action="store", help="path of file to take as input", nargs="?", metavar="file")

    args = parser.parse_args()

    print(args.file)
	
    main()	