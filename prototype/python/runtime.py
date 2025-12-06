import language

class Runtime():
    def __init__(self,ast):
        self.ast = ast
        self.environment = language.Environment()

    def run(self):
        for obj in self.ast.children:
            if isinstance(obj,language.Statement):
                obj.execute(self.environment)
