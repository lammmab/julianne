#[
ast.nim
Lammmab - 12/10/25

The main AST structure of the program:
  - Contains all node objects
  - Methods for pretty-printing an AST
  - Automated error handling

Usage:
  1. Parse a stream of valid Julianne tokens
  2. Call ast_transformer(tokens)

Todo:
1. Fix lt_formatted_str case
2. Finish class implementation
3. implement nk_member (member access b.c.d)

4. better node factory?

]#
include token_defs
include ast_defs

import strutils

proc link_relationship(parent: ref ASTNode, child: ref ASTNode) =
  if parent != nil and child != nil:
    parent.children.add(child)
    child.parent = parent

proc link_relationship_multiple(parent: ref ASTNode, children: seq[ref ASTNode]) =
  for child in children:
    link_relationship(parent,child)

proc new_node(parent: ref ASTNode, kind: NodeKind): ref ASTNode =
  let n = new ASTNode
  link_relationship(parent,n)
  n.kind = kind
  n

proc new_root(c: seq[ref ASTNode]): ref ASTNode =
  let n = new_node(nil,nk_root_node)
  link_relationship_multiple(n,c)
  n

proc new_call(target: ref ASTNode, args: seq[ref ASTNode], p: ref ASTNode): ref ASTNode =
  let n = new_node(p,nk_call)
  n.call = Call(target: target, args: args)
  link_relationship(n, target)
  link_relationship_multiple(n, args)
  n

proc new_index_access(target, index, p: ref ASTNode): ref ASTNode =
  let n = new_node(p, nk_index)
  n.access = IndexAccess(target: target, index: index)
  link_relationship(n, target)
  link_relationship(n, index)
  n

proc new_return(val: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let n = new_node(p, nk_return)
  n.val = val
  link_relationship(n, val)
  n

proc new_var_assignment(i: ref ASTNode, n_v: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let n = new_node(p, nk_var_assign)
  n.var_assignment = Assignment(identifier: i, new_value: n_v)
  link_relationship(n, i)
  link_relationship(n, n_v)
  n

proc new_while(cond: ref ASTNode, body: seq[ref ASTNode], p: ref ASTNode): ref ASTNode =
  let n = new_node(p,nk_while)
  n.loop = WhileDeclaration(cond: cond, body: body)
  link_relationship(n, cond)
  link_relationship_multiple(n, body)
  n

proc new_func(identifier: string, params: seq[Param], body: seq[ref ASTNode], p: ref ASTNode): ref ASTNode =
  let n = new_node(p,nk_func_decl)
  n.func_decl = FunctionDeclaration(
    identifier: identifier,
    params: params,
    body: body
  )

  link_relationship_multiple(n,body)
  n

proc match_if(text: string): IfType =
  if text == "if":
    return it_if
  elif text == "elseif":
    return it_elseif
  elif text == "else":
    return it_else
  else:
    raise newException(ValueError, "Invalid if keyword: " & text)


proc new_if_branch(statement: ref ASTNode, if_decl: IfDeclaration) =
  statement.if_chain.branches.add(if_decl)

proc new_if_chain(branch: IfDeclaration): ref ASTNode =
  let chain = new ASTNode
  chain.kind = nk_if_chain
  chain.if_chain = IfChain(branches: @[])
  chain.if_chain.branches.add(branch)
  chain

proc new_var(name: string, initializer: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let n = new_node(p,nk_var_decl)
  n.var_decl = VariableDeclaration(identifier: name, initializer: initializer)
  link_relationship(n,initializer)
  n

proc new_ident(name: string,p: ref ASTNode): ref ASTNode =
  let n = new_node(p,nk_identifier)
  n.identifier_name = name
  n

proc new_lit[T](v: T, p: ref ASTNode): ref ASTNode =
  let val: LiteralValue = 
    when T is int: LiteralValue(kind: lt_int, int_val: v)
    elif T is float: LiteralValue(kind: lt_float, float_val: v)
    elif T is string: LiteralValue(kind: lt_str, str_val: v)
    elif T is seq[ref ASTNode]: LiteralValue(kind: lt_formatted_str, formatted_str_val: v)
    elif T is ArrayLiteral: LiteralValue(kind: lt_arr, arr_val: v)
    else: static: doAssert false, "Unsupported literal type"

  let n = new_node(p,nk_literal)
  n.literal = val
  n
  
proc new_unary_operation(op: string, left: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let n = new_node(p,nk_operation)
  n.operation = Operation(op: op, left: left)
  link_relationship(n, left)
  n

proc new_binary_operation(op: string, left: ref ASTNode, right: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let n = new_node(p,nk_operation)
  n.operation = Operation(op: op, left: left, right: right)
  link_relationship(n, left)
  link_relationship(n, right)
  n

let operator_precedence_seq: seq[tuple[id: string, number: int]] = @[
  ("^", 12), ("~", 11), ("*", 10), ("/", 10), ("%", 10),
  ("+", 9), ("-", 9), ("<", 7), (">", 7), ("<=", 7),
  (">=", 7), ("~=", 7), ("==", 7), ("&", 6), ("|", 5)
]

proc get_precedence(op: string): int =
  for item in operator_precedence_seq:
    if item.id == op:
      return item.number
  return 1

type
  TransformerContext = object
    idx: int
    tokens: seq[JulianneToken]

func current_token(c: TransformerContext): JulianneToken =
  return c.tokens[c.idx]

func advance_token(c: var TransformerContext): JulianneToken {.discardable.} =
  let cur = c.current_token()
  c.idx += 1
  return cur

proc match_kind(ctx: var TransformerContext, kind: TokenKind): bool = 
  if ctx.idx < ctx.tokens.len and ctx.tokens[ctx.idx].kind == kind: 
    ctx.advance_token()
    return true 
  else: 
    return false

proc match_kind_multiple(
  ctx: var TransformerContext,
  kinds: varargs[TokenKind]
): bool =
  let tok_kind = ctx.current_token().kind
  for kind in kinds:
    if kind == tok_kind:
      return true
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

proc parse_parenthesized_exp(
  ctx: var TransformerContext
): ref ASTNode =
  match_kind_err(ctx,tk_paren_open,"Missing opening '('")
  let exp: ref ASTNode = parse_expression(ctx,0,true)
  match_kind_err(ctx,tk_paren_close,"Missing closing ')'")
  exp

proc parse_nested_block(
  ctx: var TransformerContext
): seq[ref ASTNode] =
  var nodes: seq[ref ASTNode]
  match_kind_err(ctx,tk_brace_open,"Missing '{' for nested block")
  while ctx.idx < ctx.tokens.len and ctx.current_token().kind != tk_brace_close:
    nodes.add(parse_statement(ctx))
  match_kind_err(ctx,tk_brace_close, "Missing '}' for inline block")
  nodes

proc parse_base_primary*(ctx: var TransformerContext, expect_unary: bool): ref ASTNode =
  if ctx.idx >= ctx.tokens.len:
    raise newException(ValueError, "Unexpected end of input")

  let tok = ctx.advance_token()

  case tok.kind
  of tk_int_literal:
    return new_lit(parseInt(tok.text), nil)
  of tk_fl_literal:
    return new_lit(parseFloat(tok.text), nil)
  of tk_var_identifier, tk_const_identifier:
    return new_ident(tok.text, nil)
  of tk_operator:
    if expect_unary:
      let operand = parse_primary(ctx,true)
      return new_unary_operation(tok.text, operand, nil)
    else:
        raise newException(ValueError, "Unexpected operator: " & tok.text)
  of tk_paren_open:
    let expression = parse_expression(ctx,0,true)
    match_kind_err(ctx,tk_paren_close,"Expected closing ')'")
    return expression
  of tk_bracket_open:
    var elements: seq[ref ASTNode] = @[]
    while ctx.idx < ctx.tokens.len and ctx.current_token().kind != tk_bracket_close:
      elements.add(parse_expression(ctx,0,true))
      if ctx.current_token().kind == tk_comma:
        ctx.advance_token()
    match_kind_err(ctx,tk_bracket_close,"Expected closing ']' for array")
    return new_lit(ArrayLiteral(elements: elements), nil)
  of tk_string_normal,tk_string_raw,tk_string_formatted:
    return new_lit(tok.text,nil)
  else:
    raise newException(ValueError, "Unexpected token: " & $tok.kind)

proc parse_postfix(
  ctx: var TransformerContext,
  exp: ref ASTNode
): ref ASTNode =
  var expression = exp

  while ctx.idx < ctx.tokens.len:
    let tok = ctx.current_token()

    case tok.kind
    of tk_bracket_open:
      ctx.advance_token()
      let idx_expr = parse_expression(ctx, 0, true)
      match_kind_err(ctx, tk_bracket_close, "Expected ']' after index expression")

      let id_access = new_index_access(expression, idx_expr, nil)
      expression = id_access
    of tk_paren_open:
      ctx.advance_token()
      var args: seq[ref ASTNode] = @[]
      while ctx.idx < ctx.tokens.len and ctx.current_token().kind != tk_paren_close:
        args.add(parse_expression(ctx, 0, true))
        if ctx.current_token().kind == tk_comma:
          ctx.idx += 1
      match_kind_err(ctx, tk_paren_close, "Expected ')' after function call arguments")
      expression = new_call(expression, args, nil)
    else:
      break

  return expression

proc parse_primary*(ctx: var TransformerContext, expect_unary: bool): ref ASTNode =
  var expression = parse_base_primary(ctx, expect_unary)
  return parse_postfix(ctx, expression)

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
    if ctx.current_token().kind == tk_operator: 
      break
    let right = parse_expression(ctx, prec + 1, true)
    left = new_binary_operation(tok.text, left, right, nil)
    allow_unary = false
  return left

proc parse_var_decl*(
    ctx: var TransformerContext
): ref ASTNode =
  let var_type = ctx.advance_token().text
  let identifier = ctx.advance_token().text

  match_kind_err(ctx,tk_assign,"Missing equals sign for variable declaration")
  let exp = parse_expression(ctx,0,true)
  let variable = new_var(identifier,exp,nil)
  variable
  
proc parse_if*(
  ctx: var TransformerContext
): IfDeclaration =
  let if_type = match_if(ctx.current_token().text)
  ctx.idx += 1

  var exp: ref ASTNode
  if if_type != it_else:
    exp = parse_parenthesized_exp(ctx)

  let nodes = parse_nested_block(ctx)
  return IfDeclaration(if_type: if_type, cond: exp, body: nodes)

proc parse_if_statement*(
  ctx: var TransformerContext
): ref ASTNode =
  var if_decl = parse_if(ctx)
  var statement = new_if_chain(if_decl)
  while ctx.idx < ctx.tokens.len and (ctx.current_token().text == "elseif" or ctx.current_token().text == "else"):
    new_if_branch(statement,parse_if(ctx))
  statement

proc parse_params(
  ctx: var TransformerContext
): seq[Param] =
  var params: seq[Param] = @[]

  while ctx.idx < ctx.tokens.len and ctx.current_token().kind != tk_paren_close:
    let tok = ctx.current_token()

    if tok.kind notin {tk_var_identifier, tk_const_identifier}:
      raise newException(ValueError, "Expected parameter name, got: " & tok.text)

    params.add(Param(name: tok.text))
    ctx.idx += 1
    if ctx.current_token().kind != tk_comma and ctx.current_token().kind != tk_paren_close:
      raise newException(ValueError, "Expected ',' or ')' in parameter list, got: " & tok.text)
    elif ctx.current_token().kind == tk_comma:
      ctx.idx += 1
    else:
      continue
  

  match_kind_err(ctx,tk_paren_close, "Expected ')' after parameter list")

  return params

proc parse_fn_decl*(
  ctx: var TransformerContext
): ref ASTNode =
  ctx.idx += 1

  let tok = ctx.current_token()
  if tok.kind notin {tk_var_identifier, tk_const_identifier}:
    raise newException(ValueError, "Expected function name")
  let name = tok.text
  ctx.idx += 1

  match_kind_err(ctx, tk_paren_open, "Missing '(' after function name")
  let params = parse_params(ctx)
  let nodes = parse_nested_block(ctx)
  return new_func(name,params,nodes,nil)

proc parse_while_decl*(
    ctx: var TransformerContext
): ref ASTNode =
  ctx.advance_token()
  let exp = parse_parenthesized_exp(ctx)
  let child_nodes = parse_nested_block(ctx)
  let while_loop = new_while(exp,child_nodes,nil)
  while_loop

proc parse_return(
    ctx: var TransformerContext
): ref ASTNode =
  ctx.advance_token()
  var exp: ref ASTNode
  if not match_kind_multiple(ctx,tk_seperator,tk_brace_close):
    exp = parse_expression(ctx,0,true)
  return new_return(exp,nil)

proc parse_object_decl(
  ctx: var TransformerContext
): ref ASTNode =
  ctx.advance_token()

  let ident: string = ctx.advance_token().text
  let nodes = parse_nested_block(ctx)
  raise newException(ValueError, "Classes not implemented")
  

proc parse_keyword(ctx: var TransformerContext): ref ASTNode =
  case ctx.current_token().text
  of "let","var","const":
    return parse_var_decl(ctx)
  of "if":
    return parse_if_statement(ctx)
  of "fn":
    return parse_fn_decl(ctx)
  of "while":
    return parse_while_decl(ctx)
  of "return":
    return parse_return(ctx)
  of "class":
    return parse_object_decl(ctx)

proc parse_expression_statement(ctx: var TransformerContext): ref ASTNode =
  let lhs = parse_expression(ctx,0,true)
  if ctx.idx < ctx.tokens.len and ctx.current_token().kind == tk_assign:
    ctx.idx += 1
    let rhs = parse_expression(ctx,0,true)
    
    case lhs.kind
    of nk_identifier,nk_index:
      return new_var_assignment(lhs,rhs,nil)
    else:
      let text = "Invalid assignment target: " % $(lhs.kind)
      raise newException(ValueError, text)
  return lhs

proc parse_statement(ctx: var TransformerContext): ref ASTNode =
  let tok = ctx.current_token()
  var statement: ref ASTNode

  case tok.kind
  of tk_keyword:
    statement = parse_keyword(ctx)
  else:
    statement = parse_expression_statement(ctx)
  
  if ctx.idx < ctx.tokens.len and ctx.current_token().kind == tk_seperator:
    ctx.idx += 1

  return statement

proc ast_transformer*(tokens: seq[JulianneToken]): ref ASTNode =
  var ctx = TransformerContext(
    idx: 0,
    tokens: tokens
  )

  var statements: seq[ref ASTNode] = @[]
  while ctx.idx < ctx.tokens.len:
    echo "Parsing: ", ctx.current_token().text
    if ctx.current_token().kind == tk_seperator:
      ctx.idx += 1
      continue

    let statement = parse_statement(ctx)
    statements.add(statement)
  return new_root(statements)