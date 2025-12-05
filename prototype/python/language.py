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

    def set(self, immutable, k, v):
        if k in self.values:
            self.values[k].set(v)
        else:
            self.values[k] = Variable(immutable, v)

class Variable():
    def __init__(self,immutable,value):
        self.immutable = immutable == True
        self.value = value

    def get(self):
        return self.value

    def set(self,v):
        if not self.immutable:
            self.value = v
        else:
            raise PermissionError("Attempt to modify immutable value")

class Expression():
    def evaluate(self,_):
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