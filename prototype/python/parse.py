from lark import Lark

def get_file_contents(filepath):
    try:
        with open(filepath, 'r') as file:
            return file.read()
    except FileNotFoundError as e:
        raise FileNotFoundError(f"File not found: {e}")
    except IOError as e:
        raise IOError(f"IOError: {e}")

class Parser():
    def __init__(self,transformer,path="grammar.lark"):
        self.path = path
        self.parser = Lark(get_file_contents(self.path),parser="lalr",transformer=transformer)

    def parse(self,filepath):
        return self.parser.parse(get_file_contents(filepath))