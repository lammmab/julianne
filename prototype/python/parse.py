from lark import Lark

def get_file_contents(filepath):
    try:
        with open(filepath, 'r') as file:
            content = file.read()
    except FileNotFoundError:
        print(f"Error: The file at {filepath} was not found.")
    except IOError as e:
        print(f"Error: An I/O error occurred: {e}")
    

parser = Lark(get_file_contents("grammar.lark"),parser="lalr",transformer=None)
def parse(filepath):
    content = get_file_contents(filepath)
    if content:
        return parser.parse(content)