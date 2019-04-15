# antlr milestone_1.g4 -Dlanguage=Python3
import argparse
from antlr4 import *
from milestone_1Lexer import milestone_1Lexer
from milestone_1Listener import milestone_1Listener
from milestone_1Parser import milestone_1Parser
from antlr4.tree.Trees import Trees

def main():
    with open(args.file, "r") as file:
        lines = file.read()
    input_stream = InputStream(lines)
    lexer = milestone_1Lexer(input_stream)

    with open("milestone_1.tokens", "r") as file:
        lines = file.readlines()
    lines.reverse()
    token_dict = dict((line.strip().split('=')[1], line.split('=')[0]) for line in lines)

    # token_stream = CommonTokenStream(lexer)
    # parser = milestone_1Parser(token_stream)

    # tree = parser.start()
    # print(Trees.toStringTree(tree,None, parser))

    token = lexer.nextToken()

    output_file = open("milestone_1_result.txt", "w+")
    while not token.type == Token.EOF:
        if str(token.type)!="NONE":
            output_file.write(token_dict[str(token.type)] + ' ' +  token.text + '\n')
            print(token_dict[str(token.type)], token.text)
        token = lexer.nextToken()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(add_help=True, description='Sample Commandline')

    parser.add_argument('--file', action="store", help="path of file to take as input", nargs="?", metavar="file")

    args = parser.parse_args()

    print(args.file)
	
    main()	