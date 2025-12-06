from ast_transformer import ASTTransformer
from parse import Parser
from sys import argv,set_int_max_str_digits
from runtime import Runtime

def main(filepath):
    set_int_max_str_digits(100000)
    JJParser = Parser(ASTTransformer())
    parsed_contents = JJParser.parse(filepath)
    runtime = Runtime(parsed_contents)
    runtime.run()

if __name__ == "__main__":
    if len(argv) == 1: raise TypeError("No filepath provided; cannot interpret")
    filepath = argv[1]
    if not filepath.lower().endswith(".jj"): raise TypeError("Filepath provided does not have Julianne file extension")
    main(argv[1])