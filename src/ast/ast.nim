import strutils

type
  LiteralType* = enum
    nkFloat, nkInteger, nkString, nkArr, nkBool

  AstNodeType* = enum
    nkLiteral, nkBinOp, nkIdentifier, nkFunctionCall

  AstNode* = ref object of RootObj
    kind*: AstNodeType
    line*, col*: int
    parent*: AstNode

  LiteralNode* = ref object of AstNode
    case literal_type*: LiteralType
    of nkInteger:
      i_v*: int
    of nkFloat:
      f_v*: float
    of nkString:
      s_v*: string
    of nkArr:
      a_v*: seq[AstNode]
    of nkBool:
      b_v*: bool

  IdentifierNode* = ref object of AstNode
    name*: string

type
  ExprKind* = enum
    ekLiteral, ekIdentifier, ekBinaryOp, ekUnaryOp, ekFunctionCall, ekArrayAccess, ekMemberAccess

  ExprNode* = ref object of AstNode
    case kindEx*: ExprKind
    of ekLiteral:
      lit*: LiteralNode
    of ekIdentifier:
      id*: IdentifierNode
    of ekBinaryOp:
      left*, right*: ExprNode
      bin_op*: string
    of ekUnaryOp:
      expression*: ExprNode
      unary_op*: string
    of ekFunctionCall:
      callee*: ExprNode
      args*: seq[ExprNode]
    of ekArrayAccess:
      arr*, index*: ExprNode
    of ekMemberAccess:
      base*: ExprNode
      property*: string

type
  StmtKind* = enum
    skVarDecl, skConstDecl, skAssign, skReturn,
    skIf, skWhile, skExprStmt, skFunctionDecl, skBlock

  StmtNode* = ref object of AstNode
    case kindStmt*: StmtKind
    of skVarDecl, skConstDecl:
      declared_name*: string
      declared_value*: ExprNode
    of skAssign:
      target*: ExprNode
      assigned_value*: ExprNode
    of skReturn:
      returned_value*: ExprNode
    of skIf:
      if_cond*: ExprNode
      thenBlock*: StmtNode
      elseBlock*: StmtNode
    of skWhile:
      while_cond*: ExprNode
      while_body*: StmtNode
    of skExprStmt:
      expres*: ExprNode
    of skFunctionDecl:
      func_declared_name*: string
      params*: seq[string]
      func_body*: StmtNode
    of skBlock:
      stmts*: seq[StmtNode]

type
  ParseContext* = object
    astStack*: seq[AstNode]

var ctx*: ParseContext

proc push_node*(n: AstNode) =
  ctx.astStack.add(n)

proc pop_node*(): AstNode =
  echo "Node being captured"
  result = ctx.astStack[^1]
  ctx.astStack.del(ctx.astStack.high)

# === Literal constructors ===
proc new_bool_literal*(value: bool): LiteralNode =
  LiteralNode(kind: nkLiteral, literal_type: nkBool, b_v: value)

proc new_int_literal*(value: int): LiteralNode =
  LiteralNode(kind: nkLiteral, literal_type: nkInteger, i_v: value)

proc new_float_literal*(value: float): LiteralNode =
  LiteralNode(kind: nkLiteral, literal_type: nkFloat, f_v: value)

proc new_str_literal*(value: string): LiteralNode =
  LiteralNode(kind: nkLiteral, literal_type: nkString, s_v: value)

proc new_arr_literal*(value: seq[AstNode]): LiteralNode =
  LiteralNode(kind: nkLiteral, literal_type: nkArr, a_v: value)

# === Expr constructors ===
proc new_literal_expr*(lit: LiteralNode): ExprNode =
  ExprNode(kindEx: ekLiteral, lit: lit)

proc new_bool_expr*(value: bool): ExprNode =
  new_literal_expr(new_bool_literal(value))

proc new_int_expr*(value: int): ExprNode =
  new_literal_expr(new_int_literal(value))

proc new_float_expr*(value: float): ExprNode =
  new_literal_expr(new_float_literal(value))

proc new_str_expr*(value: string): ExprNode =
  new_literal_expr(new_str_literal(value))

proc new_arr_expr*(value: seq[AstNode]): ExprNode =
  new_literal_expr(new_arr_literal(value))

proc new_identifier_expr*(name: string): ExprNode =
  ExprNode(kindEx: ekIdentifier, id: IdentifierNode(kind: nkIdentifier, name: name))

proc new_binary_expr*(left: ExprNode, op: string, right: ExprNode): ExprNode =
  ExprNode(kindEx: ekBinaryOp, left: left, bin_op: op, right: right)

proc new_unary_expr*(expr: ExprNode, op: string): ExprNode =
  ExprNode(kindEx: ekUnaryOp, expression: expr, unary_op: op)

proc new_function_call_expr*(callee: ExprNode, args: seq[ExprNode]): ExprNode =
  ExprNode(kindEx: ekFunctionCall, callee: callee, args: args)

proc new_array_access_expr*(arr, index: ExprNode): ExprNode =
  ExprNode(kindEx: ekArrayAccess, arr: arr, index: index)

# === Stmt constructors ===
proc new_var_decl*(name: string, value: ExprNode): StmtNode =
  StmtNode(kindStmt: skVarDecl, declared_name: name, declared_value: value)

proc new_const_decl*(name: string, value: ExprNode): StmtNode =
  StmtNode(kindStmt: skConstDecl, declared_name: name, declared_value: value)

proc new_assign*(target: ExprNode, value: ExprNode): StmtNode =
  StmtNode(kindStmt: skAssign, target: target, assigned_value: value)

proc new_return*(value: ExprNode): StmtNode =
  StmtNode(kindStmt: skReturn, returned_value: value)

proc new_expr_stmt*(expr: ExprNode): StmtNode =
  StmtNode(kindStmt: skExprStmt, expres: expr)

proc new_if_stmt*(cond: ExprNode, thenBlock, elseBlock: StmtNode): StmtNode =
  StmtNode(kindStmt: skIf, if_cond: cond, thenBlock: thenBlock, elseBlock: elseBlock)

proc new_while_stmt*(cond: ExprNode, body: StmtNode): StmtNode =
  StmtNode(kindStmt: skWhile, while_cond: cond, while_body: body)

proc new_block*(stmts: seq[StmtNode]): StmtNode =
  StmtNode(kindStmt: skBlock, stmts: stmts)

proc new_function_decl*(name: string, params: seq[string], body: StmtNode): StmtNode =
  StmtNode(kindStmt: skFunctionDecl, func_declared_name: name, params: params, func_body: body)

proc new_member_access_expr*(base: ExprNode, property: string): ExprNode =
  ExprNode(kindEx: ekMemberAccess, base: base, property: property)

# === Push helpers ===
proc push_bool_expr*(value: bool) = push_node(new_bool_expr(value))
proc push_int_expr*(value: int) = push_node(new_int_expr(value))
proc push_float_expr*(value: float) = push_node(new_float_expr(value))
proc push_str_expr*(value: string) = push_node(new_str_expr(value))
proc push_arr_expr*(value: seq[AstNode]) = push_node(new_arr_expr(value))
proc push_identifier_expr*(name: string) = push_node(new_identifier_expr(name))
proc push_binary_expr*(left: ExprNode, op: string, right: ExprNode) = push_node(new_binary_expr(left, op, right))
proc push_unary_expr*(expr: ExprNode, op: string) = push_node(new_unary_expr(expr, op))
proc push_function_call_expr*(callee: ExprNode, args: seq[ExprNode]) = push_node(new_function_call_expr(callee, args))
proc push_array_access_expr*(arr, index: ExprNode) = push_node(new_array_access_expr(arr, index))
proc push_var_decl*(name: string, value: ExprNode) = push_node(new_var_decl(name, value))
proc push_const_decl*(name: string, value: ExprNode) = push_node(new_const_decl(name, value))
proc push_assign*(target, value: ExprNode) = push_node(new_assign(target, value))
proc push_return*(value: ExprNode) = push_node(new_return(value))
proc push_expr_stmt*(expr: ExprNode) = push_node(new_expr_stmt(expr))
proc push_if_stmt*(cond: ExprNode, thenBlock, elseBlock: StmtNode) = push_node(new_if_stmt(cond, thenBlock, elseBlock))
proc push_while_stmt*(cond: ExprNode, body: StmtNode) = push_node(new_while_stmt(cond, body))
proc push_block*(stmts: seq[StmtNode]) = push_node(new_block(stmts))
proc push_function_decl*(name: string, params: seq[string], body: StmtNode) = push_node(new_function_decl(name, params, body))

proc dump_node*(node: AstNode, indent: int = 0) =
  if node == nil:
    echo repeat(" ", indent) & "<nil>"
    return
  let pad = repeat(" ", indent)

  if node of ExprNode:
    let e = ExprNode(node)
    case e.kindEx
    of ekLiteral:
      let lit = e.lit
      case lit.literal_type
      of nkInteger: echo pad & "Literal[int]: ", lit.i_v
      of nkFloat:   echo pad & "Literal[float]: ", lit.f_v
      of nkString:  echo pad & "Literal[string]: \"", lit.s_v, "\""
      of nkBool:    echo pad & "Literal[bool]: ", lit.b_v
      of nkArr:
        echo pad & "Literal[array]: ["
        for child in lit.a_v:
          dump_node(child, indent + 2)
        echo pad & "]"
    of ekIdentifier:
      echo pad & "Identifier: ", e.id.name
    of ekBinaryOp:
      echo pad & "BinaryOp: ", e.bin_op
      dump_node(e.left, indent + 2)
      dump_node(e.right, indent + 2)
    of ekUnaryOp:
      echo pad & "UnaryOp: ", e.unary_op
      dump_node(e.expression, indent + 2)
    of ekFunctionCall:
      echo pad & "FunctionCall:"
      dump_node(e.callee, indent + 2)
      echo pad & "Args:"
      for arg in e.args:
        dump_node(arg, indent + 4)
    of ekArrayAccess:
      echo pad & "ArrayAccess:"
      echo pad & "Array:"
      dump_node(e.arr, indent + 2)
      echo pad & "Index:"
      dump_node(e.index, indent + 2)
  elif node of StmtNode:
    let s = StmtNode(node)
    case s.kindStmt
    of skVarDecl:
      echo pad & "VarDecl: ", s.declared_name
      dump_node(s.declared_value, indent + 2)
    of skConstDecl:
      echo pad & "ConstDecl: ", s.declared_name
      dump_node(s.declared_value, indent + 2)
    of skAssign:
      echo pad & "Assign:"
      dump_node(s.target, indent + 2)
      dump_node(s.assigned_value, indent + 2)
    of skReturn:
      echo pad & "Return:"
      dump_node(s.returned_value, indent + 2)
    of skIf:
      echo pad & "IfStmt:"
      echo pad & "Condition:"
      dump_node(s.if_cond, indent + 2)
      echo pad & "Then:"
      dump_node(s.thenBlock, indent + 2)
      if s.elseBlock != nil:
        echo pad & "Else:"
        dump_node(s.elseBlock, indent + 2)
    of skWhile:
      echo pad & "WhileStmt:"
      echo pad & "Condition:"
      dump_node(s.while_cond, indent + 2)
      echo pad & "Body:"
      dump_node(s.while_body, indent + 2)
    of skExprStmt:
      echo pad & "ExprStmt:"
      dump_node(s.expres, indent + 2)
    of skFunctionDecl:
      echo pad & "FunctionDecl: ", s.func_declared_name
      echo pad & "Params: ", s.params
      echo pad & "Body:"
      dump_node(s.func_body, indent + 2)
    of skBlock:
      echo pad & "Block:"
      for stmt in s.stmts:
        dump_node(stmt, indent + 2)
  elif node of LiteralNode:
    echo pad & "LiteralNode (unexpected raw node)"
  else:
    echo pad & "Unknown node type"

proc dump_ast_stack*() =
  echo "AST Stack:"
  for i, n in ctx.astStack:
    echo "  [", i, "]"
    dump_node(n, 4)