import operator
binary_ops = {
    "+": operator.add,
    "-": operator.sub,
    "/": operator.truediv,
    "*": operator.mul,
    "^": operator.pow,
    "==": operator.eq,
    "~=": operator.ne,
    "<": operator.lt,
    "<=": operator.le,
    ">": operator.gt,
    ">=": operator.ge
}

unary_ops = {
    "-": operator.neg,
    "+": operator.abs,
    "#": lambda x: len(x)
}

class Environment():
    def __init__(self,parent=None):
        self.parent = parent
        self.values = {}

    def get(self,k):
        if k in self.values:
            return self.values[k]
        elif self.parent:
            return self.parent.get(k)
        else:
            raise NameError(f"{k} not defined")

    def set(self, var_type, immutable, k, v):
        if k in self.values:
            self.values[k].set(v)
        else:
            self.values[k] = Variable(var_type,immutable,v)

class Variable():
    def __init__(self,type,immutable,value):
        self.immutable = immutable == True
        self.value = value
        self.variable_type = type
    def get(self):
        return self.value

    def set(self,v):
        if not self.immutable:
            self.value = v
        else:
            raise PermissionError("Attempt to modify immutable value")

class Expression():
    def evaluate(self,_: Environment):
        raise NotImplementedError
    
class BinaryExpression(Expression):
    def __init__(self,left,op,right):
        self.left = left
        self.op = op
        self.right = right

    def evaluate(self,env):
        left = self.left.evaluate(env)
        right = self.right.evaluate(env)

        if not self.op in binary_ops: raise ValueError(f"{self.op} is not a valid binary operator")
        operation = binary_ops[self.op]
        return operation(left,right)

class UnaryExpression(Expression):
    def __init__(self,op,expr):
        self.op = op
        self.expr = expr

    def evaluate(self,env):
        expr = self.expr.evaluate(env)
        if not self.op in unary_ops: raise ValueError(f"{self.op} is not a valid unary operator")
        operation = unary_ops[self.op]
        return operation(expr)

class LiteralExpression(Expression):
    def __init__(self,value):
        self.value = value

    def evaluate(self,_):
        return self.value

class VariableExpression(Expression):
    def __init__(self,name):
        self.name = name

    def evaluate(self,env):
        var = env.get(self.name)
        return var.get()
    
class Statement():
    def evaluate(self, _: Environment):
        raise NotImplementedError

TYPE_MAP = {
    "int": int,
    "float": float,
    "str": str,
    "boolean": bool,
}

def assign_type(vartype,val):
    type = TYPE_MAP[vartype]
    if type is None: raise TypeError(f"Unknown type: {vartype}")
    return type(val)

class VariableAssignment(Statement):
    def __init__(self,immutable,type,name,value):
        self.immutable = immutable == "let"
        self.type = type
        self.name = name
        self.value = value

    def execute(self, env):
        evaluated = self.value.evaluate(env)
        typed_value = assign_type(self.type, evaluated)
        env.set(self.type,self.immutable, self.name, typed_value)

class OutputValue(Statement):
    def __init__(self,value):
        self.value = value

    def execute(self, env):
        print(self.value.evaluate(env))