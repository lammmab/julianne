import union/union
import std/tables
import tokenizer
import strutils
type None = object

type
  NodeKind* = enum 
    nk_root_node, 
    nk_literal, 
    nk_identifier, 
    nk_operation,
    nk_var_decl
    
  LiteralType* = enum 
    lt_str,
    lt_formatted_str,
    lt_float,
    lt_int,
    lt_arr,

  ArrayLiteral* = object
    elements*: seq[ref ASTNode]

  VariableDeclaration* = object
    identifier: string
    initializer: ref ASTNode

  LiteralValue* = object
    case kind: LiteralType
    of lt_str: str_val: string
    of lt_formatted_str: formatted_str_val: seq[ref ASTNode]
    of lt_float: float_val: float
    of lt_arr: arr_val: ArrayLiteral
    of lt_int: int_val: int

  Operation* = object
    op: string
    left: ref ASTNode
    right: ref ASTNode

  ASTNode* = object
    parent*: ref ASTNode
    children*: seq[ref ASTNode]
    case kind: NodeKind
    of nk_root_node: discard
    of nk_literal: literal: LiteralValue
    of nk_identifier: identifier_name: string
    of nk_operation: operation: Operation
    of nk_var_decl: decl: VariableDeclaration

proc new_var(name: string, initializer: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let variable = new ASTNode
  variable.parent = p
  variable.kind = nk_var_decl
  variable.decl = VariableDeclaration(identifier: name, initializer: initializer)
  if initializer != nil:
    variable.children = @[initializer]
    initializer.parent = variable
  else:
    variable.children = @[]
  variable

proc new_ident(name: string,p: ref ASTNode): ref ASTNode =
  let ident = new ASTNode
  ident.parent = p
  ident.kind = nk_identifier
  ident.identifier_name = name
  ident

proc new_lit[T](v: T, p: ref ASTNode): ref ASTNode =
  let val: LiteralValue = 
    when T is int: LiteralValue(kind: lt_int, int_val: v)
    elif T is float: LiteralValue(kind: lt_float, float_val: v)
    elif T is string: LiteralValue(kind: lt_str, str_val: v)
    elif T is seq[ref ASTNode]: LiteralValue(kind: lt_formatted_str, formatted_str_val: v)
    elif T is ArrayLiteral: LiteralValue(kind: lt_arr, arr_val: v)
    else: static: doAssert false, "Unsupported literal type"

  let litNode = new ASTNode
  litNode.parent = p
  litNode.kind = nk_literal
  litNode.literal = val
  return litNode

proc printAST*(node: ref ASTNode, prefix: string = "", isLast: bool = true) =
  let branch = if prefix.len > 0:
                 if isLast: "└─ " else: "├─ "
               else: ""
  let newPrefix = prefix & (if isLast: "   " else: "│  ")

  var label = ""
  case node.kind
  of nk_root_node:
    label = "(ROOT)"
  of nk_literal:
    case node.literal.kind
    of lt_int: label = "Literal(int: " & $node.literal.int_val & ")"
    of lt_float: label = "Literal(float: " & $node.literal.float_val & ")"
    of lt_str: label = "Literal(str: '" & node.literal.str_val & "')"
    of lt_formatted_str: label = "Literal(formatted_str)"
    of lt_arr: label = "Literal(array)"
  of nk_identifier:
    label = "Ident(" & node.identifier_name & ")"
  of nk_operation:
    label = "Op(" & node.operation.op & ")"
  of nk_var_decl:
    label = "VariableDeclaration(" & node.decl.identifier & ")"
  echo prefix & branch & label

  for i, child in node.children:
    printAST(child, newPrefix, i == node.children.len - 1)

  if node.kind == nk_operation:
    if node.operation.left != nil:
      printAST(node.operation.left, newPrefix, node.operation.right == nil)
    if node.operation.right != nil:
      printAST(node.operation.right, newPrefix, true)
  
proc new_unary_operation(op: string, left: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let un = new ASTNode
  un.parent = p
  un.kind = nk_operation
  un.operation = Operation(op: op, left: left)
  un

proc new_binary_operation(op: string, left: ref ASTNode, right: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let bin = new ASTNode
  bin.parent = p
  bin.kind = nk_operation
  bin.operation = Operation(op: op, left: left, right: right)
  bin

let operator_precedence_seq: seq[tuple[id: string, number: int]] = @[
  ("^", 12), ("~", 11), ("*", 10), ("/", 10), ("%", 10),
  ("+", 9), ("-", 9), ("<", 7), (">", 7), ("<=", 7),
  (">=", 7), ("~=", 7), ("==", 7), ("&", 6), ("|", 5)
]

proc get_precedence(op: string): int =
  for o, p in operator_precedence_seq:
    if p.id == op: return p.number
  return 1

type
  TransformerContext = object
    idx: int
    tokens: seq[JulianneToken]

func current_token(c: TransformerContext): JulianneToken =
  return c.tokens[c.idx]

proc match_kind(ctx: var TransformerContext, kind: TokenKind): bool = 
  if ctx.idx < ctx.tokens.len and ctx.tokens[ctx.idx].kind == kind: 
    ctx.idx += 1 
    return true 
  else: 
    return false

proc match_kind_err(ctx: var TransformerContext, kind: TokenKind, err: string) =
  if not match_kind(ctx,kind):
    raise newException(ValueError, err)

proc parse_primary*(ctx: var TransformerContext, expect_unary: bool): ref ASTNode
proc parse_expression*(
  ctx: var TransformerContext,
  min_prec: int,
  expect_unary: bool
): ref ASTNode
proc parse_statement(ctx: var TransformerContext): ref ASTNode

proc parse_inline_block(
    ctx: var TransformerContext,
    p: ref ASTNode
): ref ASTNode =
  match_kind_err(ctx,tk_bracket_open,"Missing '{' for nested block")
  while ctx.idx < ctx.tokens.len and ctx.current_token().kind != tk_bracket_close:
    let statement = parse_statement(ctx)
    statement.parent = p
    p.children.add(statement)
  match_kind_err(ctx,tk_bracket_close, "Missing '}' for inline block")

proc parse_primary*(ctx: var TransformerContext, expect_unary: bool): ref ASTNode =
  if ctx.idx >= ctx.tokens.len:
    raise newException(ValueError, "Unexpected end of input")

  let tok = ctx.current_token()

  case tok.kind
  of tk_int_literal:
    ctx.idx += 1
    return new_lit(parseInt(tok.text), nil)
  of tk_var_identifier, tk_const_identifier:
    ctx.idx += 1
    return new_ident(tok.text, nil)
  of tk_operator:
    if expect_unary:
      ctx.idx += 1
      let operand = parse_primary(ctx,true)
      return new_unary_operation(tok.text, operand, nil)
    else:
        raise newException(ValueError, "Unexpected operator: " & tok.text)
  of tk_paren_open:
    ctx.idx += 1
    let expression = parse_expression(ctx,0,true)
    match_kind_err(ctx,tk_paren_close,"Expected closing ')'")
    return expression
  of tk_bracket_open:
    ctx.idx += 1
    var elements: seq[ref ASTNode] = @[]
    while ctx.idx < ctx.tokens.len and ctx.current_token().kind != tk_bracket_close:
      elements.add(parse_expression(ctx,0,true))
      if ctx.current_token().kind == tk_comma:
        ctx.idx += 1
    match_kind_err(ctx,tk_bracket_close,"Expected closing ']' for array")
    return new_lit(ArrayLiteral(elements: elements), nil)
  of tk_string_normal,tk_string_raw,tk_string_formatted:
    ctx.idx += 1
    return new_lit(tok.text,nil)
  else:
    raise newException(ValueError, "Unexpected token: " & tok.text)

proc parse_expression*(
  ctx: var TransformerContext,
  min_prec: int,
  expect_unary: bool
): ref ASTNode =
  var left = parse_primary(ctx, expect_unary)
  var allow_unary = false
  while ctx.idx < ctx.tokens.len:
    let tok = ctx.current_token()
    if tok.kind != tk_operator or allow_unary:
      break
    let prec = get_precedence(tok.text)
    if prec < min_prec:
      break
    ctx.idx += 1
    let right = parse_expression(ctx, prec + 1, true)
    left = new_binary_operation(tok.text, left, right, nil)
    allow_unary = false
  return left

proc parse_var_decl*(
    ctx: var TransformerContext
): ref ASTNode =
  let var_type = ctx.current_token().text
  ctx.idx += 1
  let identifier = ctx.current_token().text
  ctx.idx += 2 # ignore the equals sign
  let exp = parse_expression(ctx,0,true)
  match_kind_err(ctx,tk_seperator,"Missing token seperator ; or \\n")
  let variable = new_var(identifier,exp,nil)
  exp.parent = variable
  variable
  
proc parse_keyword(ctx: var TransformerContext): ref ASTNode =
  case ctx.current_token().text
  of "let":
    return parse_var_decl(ctx)
  of "var":
    return parse_var_decl(ctx)
  of "const":
    return parse_var_decl(ctx)
  

proc parse_statement(ctx: var TransformerContext): ref ASTNode =
  let tok = ctx.current_token()
  case tok.kind
  of tk_keyword:
    return parse_keyword(ctx)
  else:
    let expression = parse_expression(ctx,0,true)
    match_kind_err(ctx,tk_seperator,"Missing token seperator ; or \\n")
    return expression   

proc ast_transformer*(tokens: seq[JulianneToken]): ref ASTNode =
  var ctx = TransformerContext(
    idx: 0,
    tokens: tokens
  )
  let root = new ASTNode
  root.kind = nk_root_node
  root.children = @[]
  while ctx.idx < ctx.tokens.len:
    echo "Parsing ", ctx.current_token().kind
    let statement = parse_statement(ctx)
    statement.parent = root
    root.children.add(statement)

  return root