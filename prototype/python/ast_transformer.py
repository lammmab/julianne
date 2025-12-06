from lark import Transformer
import language
# literal_expr
# variable_expr
# binary_expr
# unary_expr
# variable_assignment
# print
# VALUE
# VAR_NAME
class ASTTransformer(Transformer):
    def variable_assignment(self, items):
        var_type_name = items[0].value
        var_name = items[1].value
        expr = items[2]
        var_type = items[3].value
        return language.VariableAssignment(var_type_name, var_type, var_name, expr)
    
    def variable_expr(self,items):
        return language.VariableExpression(items[0].value)

    def unary_expr(self,items):
        op = items[0]
        val = items[1]
        return language.UnaryExpression(op.value,val)

    def binary_expr(self,items):
        left = items[0]
        op = items[1]
        right = items[2]
        return language.BinaryExpression(left,op.value,right)
    
    def int_literal(self,items):
        token = items[0]
        return language.LiteralExpression(int(token.value))

    def float_literal(self,items):
        token = items[0]
        return language.LiteralExpression(float(token.value))
    
    def str_literal(self,items):
        token = items[0]
        return language.LiteralExpression(token.value[1:-1])

    def bool_literal(self,items):
        token = items[0]
        return language.LiteralExpression(token.value == "true")

    def print(self,items):
        msg = items[0]
        return language.OutputValue(msg)
    
    def grouped_expr(self,items):
        return items[0]
