# antlr milestone_2.g4 -Dlanguage=Python3
import argparse
from antlr4 import *
from milestone_2Lexer import milestone_2Lexer
from milestone_2Listener import milestone_2Listener
from milestone_2Parser import milestone_2Parser
from antlr4.tree.Trees import Trees
from antlr4.error.ErrorListener import ErrorListener

# TESTCASES
# valid 1  ,2✅,3✅,4✅,5✅,6,  7✅,8✅,9✅,10✅,11✅
# invalid 1✅,2✅,3,4✅,5✅,6


class NimErrorListener( ErrorListener ):

    def __init__(self):
        super(NimErrorListener, self).__init__()

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        raise Exception("Oh no!!")

def main():
    with open(args.file, "r") as file:
        lines = file.read()
    input_stream = InputStream(lines)
    lexer = milestone_2Lexer(input_stream)

    output_str = "valid"

    with open("milestone_2.tokens", "r") as file:
        lines = file.readlines()
    lines.reverse()
    token_dict = dict((line.strip().split('=')[1], line.split('=')[0]) for line in lines)
    
    token_stream = CommonTokenStream(lexer)
    parser = milestone_2Parser(token_stream)
    parser.addErrorListener(NimErrorListener())
    
    try:
        tree = parser.start()
        print(Trees.toStringTree(tree,None, parser))
    except:
        print("invalid")
        output_str = "invalid"
    # finally:

    token = lexer.nextToken()
    if token.type!=Token.EOF:
        print(token)
        print("invalid")
        output_str = "invalid"
    else:
        print("valid")

    output_file = open("milestone_2_result.txt", "w+")
    output_file.write(output_str)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(add_help=True, description='Sample Commandline')

    parser.add_argument('--file', action="store", help="path of file to take as input", nargs="?", metavar="file")

    args = parser.parse_args()

    # print(args.file)
	
    main()	