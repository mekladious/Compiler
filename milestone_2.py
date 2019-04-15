# antlr milestone_2.g4 -Dlanguage=Python3
import argparse
from antlr4 import *
from milestone_2Lexer import milestone_2Lexer
from milestone_2Listener import milestone_2Listener
from milestone_2Parser import milestone_2Parser
from antlr4.tree.Trees import Trees

def main():
    with open(args.file, "r") as file:
        lines = file.read()
    input_stream = InputStream(lines)
    lexer = milestone_1Lexer(input_stream)

    with open("milestone_2.tokens", "r") as file:
        lines = file.readlines()
    lines.reverse()
    token_dict = dict((line.strip().split('=')[1], line.split('=')[0]) for line in lines)

    # token_stream = CommonTokenStream(lexer)
    # parser = milestone_1Parser(token_stream)

    # tree = parser.start()
    # print(Trees.toStringTree(tree,None, parser))

    token = lexer.nextToken()

    output_file = open("milestone_2_result.txt", "w+")
    while not token.type == Token.EOF:
        if get_token_type(token)!="NONE":
            output_file.write(token_dict[str(token.type)] + ' ' +  token.text + '\n')
            print(token_dict[str(token.type)], token.text)
        token = lexer.nextToken()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(add_help=True, description='Sample Commandline')

    parser.add_argument('--file', action="store", help="path of file to take as input", nargs="?", metavar="file")

    args = parser.parse_args()

    print(args.file)
	
    main()	