import sequtils

type
  LiteralKind* = enum lk_int, lk_float, lk_string, lk_bool, lk_array
  NodeKind* = enum nk_literal, nk_identifier, nk_binary_op, nk_function_call
  AstNode* = object
    kind*: NodeKind
    parent*: ref AstNode
    literal: ref LiteralValue
    left*, right*: ref AstNode
    operator*: string
    callee*: ref AstNode
    args*: seq[ref AstNode]
    identifier_name*: string

  LiteralValue* = object
    case kind*: LiteralKind
    of lk_int: int_value*: int
    of lk_float: float_value*: float
    of lk_string: string_value*: string
    of lk_bool: bool_value*: bool
    of lk_array: elements*: seq[ref AstNode]

proc dump_node(node: ref AstNode, indent: int = 0) =
  let pad = repeat("  ", indent)
  case node.kind
  of nk_literal:
    case node.literal.kind
    of lk_int: echo pad, "Literal(int): ", node.literal.int_value
    of lk_float: echo pad, "Literal(float): ", node.literal.float_value
    of lk_string: echo pad, "Literal(string): ", node.literal.string_value
    of lk_bool: echo pad, "Literal(bool): ", node.literal.bool_value
    of lk_array:
      echo pad, "Literal(array): ["
      for elem in node.literal.elements:
        dump_node(elem, indent + 1)
      echo pad, "]"
  of nk_identifier:
    echo pad, "Identifier: ", node.identifier_name
  of nk_binary_op:
    echo pad, "BinaryOp(", node.operator, "):"
    if node.left != nil: dump_node(node.left, indent + 1)
    if node.right != nil: dump_node(node.right, indent + 1)
  of nk_function_call:
    echo pad, "FunctionCall:"
    if node.callee != nil: dump_node(node.callee, indent + 1)
    for arg in node.args:
      dump_node(arg, indent + 1)

# --- literal constructors ---
proc new_int_literal_value(value: int): ref LiteralValue =
  let lit: ref LiteralValue = new(LiteralValue)
  lit.kind = lk_int
  lit.int_value = value
  lit

proc new_str_literal_value(value: string): ref LiteralValue =
  let lit: ref LiteralValue = new(LiteralValue)
  lit.kind = lk_string
  lit.string_value = value
  lit

proc new_int_literal*(value: int): ref AstNode =
  let node: ref AstNode = new(AstNode)
  node.kind = nk_literal
  node.literal = new_int_literal_value(value)
  node

proc new_str_literal*(value: string): ref AstNode =
  let node: ref AstNode = new(AstNode)
  node.kind = nk_literal
  node.literal = new_str_literal_value(value)
  node

proc new_identifier*(name: string): ref AstNode =
  let node: ref AstNode = new(AstNode)
  node.kind = nk_identifier
  node.identifier_name = name
  node

proc new_binary_op*(left: ref AstNode, op: string, right: ref AstNode): ref AstNode =
  let node: ref AstNode = new(AstNode)
  node.kind = nk_binary_op
  node.left = left
  node.right = right
  node.operator = op
  node

proc new_function_call*(callee: ref AstNode, args: seq[ref AstNode]): ref AstNode =
  let node: ref AstNode = new(AstNode)
  node.kind = nk_function_call
  node.callee = callee
  node.args = args
  node

template as_int_literal*(n: ref AstNode): int =
  assert n.kind == nk_literal and n.literal.kind == lk_int
  n.literal.int_value

template as_string_literal*(n: ref AstNode): string =
  assert n.kind == nk_literal and n.literal.kind == lk_string
  n.literal.string_value

template left_operand*(n: ref AstNode): ref AstNode =
  assert n.kind == nk_binary_op
  n.left

template right_operand*(n: ref AstNode): ref AstNode =
  assert n.kind == nk_binary_op
  n.right

type
  ParseContext* = object
    astStack*: seq[ref AstNode]

var ctx*: ParseContext

proc push_node*(n: ref AstNode) =
  echo "push_node called for: "
  dump_node(n,0)
  ctx.astStack.add(n)

proc pop_node*(): ref AstNode =
  let node = ctx.astStack[^1]
  echo "pop_node called for: "
  dump_node(node,0)
  ctx.astStack.del(ctx.astStack.high)
  result = node

proc dump_stack*() =
  echo "=== AST Stack Dump ==="
  for i, node in ctx.astStack:
    echo "[", i, "]"
    dump_node(node, 1)
  echo "=== End Stack Dump ==="