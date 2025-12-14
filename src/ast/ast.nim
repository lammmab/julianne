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
4. implement nk_member (member access b.c.d)
]#

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
    nk_var_decl,
    nk_if_chain,
    nk_func_decl,
    nk_while,
    nk_var_assign,

    nk_return,

    nk_index,
    nk_call
    
  Call* = object
    target: ref ASTNode
    args: seq[ref ASTNode]

  IndexAccess* = object
    target: ref ASTNode
    index: ref ASTNode

  LiteralType* = enum  
    lt_str,
    lt_formatted_str,
    lt_float,
    lt_int,
    lt_arr,

  IfType* = enum 
    it_if,
    it_elseif,
    it_else

  IfDeclaration* = object 
    if_type: IfType
    cond: ref ASTNode
    body: seq[ref ASTNode]

  WhileDeclaration* = object
    cond: ref ASTNode
    body: seq[ref ASTNode]

  IfChain* = object
    branches: seq[IfDeclaration]

  ArrayLiteral* = object
    elements*: seq[ref ASTNode]

  VariableDeclaration* = object
    identifier: string
    initializer: ref ASTNode
    
  Param* = object
    name: string
    paramType: ref ASTNode
    defaultValue: ref ASTNode

  FunctionDeclaration* = object
    identifier: string
    params: seq[Param]
    body: seq[ref ASTNode]

  Assignment* = object
    identifier: ref ASTNode
    new_value: ref ASTNode

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
    of nk_var_decl: var_decl: VariableDeclaration
    of nk_if_chain: if_chain: IfChain
    of nk_func_decl: func_decl: FunctionDeclaration
    of nk_while: loop: WhileDeclaration
    of nk_var_assign: var_assignment: Assignment
    of nk_return: val: ref ASTNode
    of nk_index: access: IndexAccess
    of nk_call: call: Call

proc new_call(target: ref ASTNode, args: seq[ref ASTNode], p: ref ASTNode): ref ASTNode =
  let n = new ASTNode
  n.parent = p
  n.kind = nk_call
  n.call = Call(target: target, args: args)
  for arg in args:
    arg.parent = n
  return n

proc new_index_access(target: ref ASTNode, index: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let n = new ASTNode
  n.parent = p
  n.kind = nk_index
  n.access = IndexAccess(target: target, index: index)
  return n

proc new_return(val: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let return_stmt = new ASTNode
  return_stmt.parent = p
  return_stmt.kind = nk_return
  return_stmt.val = val
  return_stmt

proc new_var_assignment(i: ref ASTNode, n_v: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let assign = new ASTNode
  assign.parent = p
  assign.kind = nk_var_assign
  assign.var_assignment = Assignment(identifier: i, new_value: n_v)
  assign

proc add_while_statement(n: ref ASTNode, statement: ref ASTNode) =
  n.loop.body.add(statement)
  n.children.add(statement)
  statement.parent = n

proc new_while(cond: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let while_node = new ASTNode
  while_node.parent = p
  while_node.kind = nk_while
  while_node.loop = WhileDeclaration(cond: cond, body: @[])
  while_node

proc new_func(identifier: string, params: seq[Param], body: seq[ref ASTNode], p: ref ASTNode): ref ASTNode =
  let fn_node = new ASTNode
  fn_node.parent = p
  fn_node.kind = nk_func_decl
  fn_node.func_decl = FunctionDeclaration(
    identifier: identifier,
    params: params,
    body: body
  )

  fn_node.children = @[]

  for statement in body:
    if statement != nil:
      statement.parent = fn_node
      fn_node.children.add(statement)

  return fn_node

proc match_if(text: string): IfType =
  if text == "if":
    return it_if
  elif text == "elseif":
    return it_elseif
  elif text == "else":
    return it_else

proc new_if_branch(statement: ref ASTNode, if_decl: IfDeclaration) =
  statement.if_chain.branches.add(if_decl)

proc new_if_chain(branch: IfDeclaration): ref ASTNode =
  let chain = new ASTNode
  chain.kind = nk_if_chain
  chain.if_chain = IfChain(branches: @[])
  chain.if_chain.branches.add(branch)
  chain

proc new_var(name: string, initializer: ref ASTNode, p: ref ASTNode): ref ASTNode =
  let variable = new ASTNode
  variable.parent = p
  variable.kind = nk_var_decl
  variable.var_decl = VariableDeclaration(identifier: name, initializer: initializer)
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
    label = "Identifier(" & node.identifier_name & ")"
  of nk_operation:
    label = "Operator(" & node.operation.op & ")"
  of nk_var_decl:
    label = "VariableDeclaration(" & node.var_decl.identifier & ")"
  of nk_call:
    label = "FunctionCall"
    echo prefix & branch & label
    if node.call.target != nil:
      echo newPrefix & "├─ Target"
      printAST(node.call.target, newPrefix & "│  ", true)
    if node.call.args.len > 0:
      echo newPrefix & "└─ Args"
      for i, arg in node.call.args:
        printAST(arg, newPrefix & "   ", i == node.call.args.len - 1)
    return
  of nk_if_chain:
    label = "IfChain"
    echo prefix & branch & label

    for i, br in node.if_chain.branches:
      let isLastBranch = i == node.if_chain.branches.len - 1
      let branchPrefix = newPrefix & (if not isLastBranch: "├─ " else: "└─ ")

      let t =
        case br.if_type
        of it_if: "if"
        of it_elseif: "elseif"
        of it_else: "else"

      echo branchPrefix & t

      if br.if_type != it_else:
        printAST(br.cond, newPrefix & (if not isLastBranch: "│  " else: "   "), true)

      if br.body.len > 0:
        echo newPrefix & (if not isLastBranch: "│  " else: "   ") & "Body"
        for j, stmt in br.body:
          printAST(stmt, newPrefix & (if not isLastBranch: "│  " else: "   ") & "   ", j == br.body.len - 1)
    return
  of nk_func_decl:
    label = "FunctionDeclaration(" & node.func_decl.identifier & ")"
    echo prefix & branch & label
    if node.func_decl.params.len > 0:
      echo newPrefix & "├─ Params"
      for i, p in node.func_decl.params:
        let isLastParam = i == node.func_decl.params.len - 1
        let pfx = newPrefix & (if not isLastParam: "│  " else: "   ")
        echo pfx & (if isLastParam: "└─ " else: "├─ ") & "Param(" & p.name & ")"
        if p.defaultValue != nil:
          printAST(
            p.defaultValue,
            pfx & (if isLastParam: "   " else: "│  "),
            true
          )
    if node.func_decl.body.len > 0:
      echo newPrefix & "└─ Body"
      for i, stmt in node.func_decl.body:
        printAST(stmt, newPrefix & "   ", i == node.func_decl.body.len - 1)
    return
  of nk_while:
    label = "WhileLoop"
    echo prefix & branch & label
    if node.loop.cond != nil:
      echo newPrefix & "├─ Condition"
      printAST(node.loop.cond, newPrefix & "│  ", true)
    if node.loop.body.len > 0:
      echo newPrefix & "└─ Body"
      for i, stmt in node.loop.body:
        printAST(stmt, newPrefix & "   ", i == node.loop.body.len - 1)
    return
  of nk_var_assign:
    label = "VariableAssignment"
    echo prefix & branch & label

    echo newPrefix & "├─ Target"
    printAST(node.var_assignment.identifier, newPrefix & "│  ", true)

    if node.var_assignment.new_value != nil:
      echo newPrefix & "└─ Value"
      printAST(node.var_assignment.new_value, newPrefix & "   ", true)
    return
  of nk_return:
    label = "Return"
    echo prefix & branch & label
    if node.val != nil:
      echo newPrefix & "└─ Value"
      printAST(node.val, newPrefix & "   ", true)
    return
  of nk_index:
    label = "IndexAccess"
    echo prefix & branch & label

    if node.access.target != nil:
      echo newPrefix & "├─ Target"
      printAST(node.access.target, newPrefix & "│  ", true)

    if node.access.index != nil:
      echo newPrefix & "└─ Index"
      printAST(node.access.index, newPrefix & "   ", true)

    return
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
): seq[ref ASTNode] =
  var nodes: seq[ref ASTNode]
  match_kind_err(ctx,tk_brace_open,"Missing '{' for nested block")
  while ctx.idx < ctx.tokens.len and ctx.current_token().kind != tk_brace_close:
    let statement = parse_statement(ctx)
    statement.parent = p
    p.children.add(statement)
    nodes.add(statement)
  match_kind_err(ctx,tk_brace_close, "Missing '}' for inline block")
  nodes

proc parse_base_primary*(ctx: var TransformerContext, expect_unary: bool): ref ASTNode =
  if ctx.idx >= ctx.tokens.len:
    raise newException(ValueError, "Unexpected end of input")

  let tok = ctx.current_token()

  case tok.kind
  of tk_int_literal:
    ctx.idx += 1
    return new_lit(parseInt(tok.text), nil)
  of tk_fl_literal:
    ctx.idx += 1
    return new_lit(parseFloat(tok.text), nil)
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
      ctx.idx += 1
      let idxExpr = parse_expression(ctx, 0, true)
      match_kind_err(ctx, tk_bracket_close, "Expected ']' after index expression")

      let newNode = new_index_access(expression, idxExpr, nil)
      idxExpr.parent = newNode
      expression = newNode
    of tk_paren_open:
      ctx.idx += 1
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
  let var_type = ctx.current_token().text # start using this var_type for consts vs privs vs immutables later
  ctx.idx += 1
  let identifier = ctx.current_token().text
  ctx.idx += 1
  match_kind_err(ctx,tk_assign,"Missing equals sign for variable declaration")
  let exp = parse_expression(ctx,0,true)
  let variable = new_var(identifier,exp,nil)
  exp.parent = variable
  variable
  
proc parse_if*(
  ctx: var TransformerContext
): IfDeclaration =
  let if_type = match_if(ctx.current_token().text)
  ctx.idx += 1

  var exp: ref ASTNode
  if if_type != it_else:
    match_kind_err(ctx,tk_paren_open,"Missing opening '(' for if statement")
    exp = parse_expression(ctx,0,true)
    match_kind_err(ctx,tk_paren_close,"Missing closing ')' for if statement")

  match_kind_err(ctx,tk_brace_open,"Missing opening '{' for if statement")
  var decl = IfDeclaration(if_type: if_type, cond: exp, body: @[])
  while ctx.idx < ctx.tokens.len and ctx.current_token().text != "}":
    let statement = parse_statement(ctx)
    decl.body.add(statement)
  match_kind_err(ctx,tk_brace_close,"Missing closing '}' for if statement")
  decl

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
  let fn = new_func(name,params,@[],nil)
  let nodes = parse_inline_block(ctx,fn)
  fn.func_decl.body = nodes

  fn

proc parse_while_decl*(
    ctx: var TransformerContext
): ref ASTNode =
  ctx.idx += 1
  match_kind_err(ctx,tk_paren_open,"Missing opening '(' for while loop")
  let exp = parse_expression(ctx,0,true)
  match_kind_err(ctx,tk_paren_close,"Missing closing ')' for while loop")
  let while_loop = new_while(exp,nil)
  let child_nodes = parse_inline_block(ctx,while_loop)
  while_loop.loop.body = child_nodes
  while_loop

proc parse_return(
    ctx: var TransformerContext
): ref ASTNode =
  ctx.idx += 1 # consume the return
  var exp: ref ASTNode
  if not match_kind(ctx,tk_seperator): # idk this is probably right
    exp = parse_expression(ctx,0,true)
  return new_return(exp,nil)


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
  let root = new ASTNode
  root.kind = nk_root_node
  root.children = @[]
  while ctx.idx < ctx.tokens.len:
    echo "Parsing: ", ctx.current_token().text
    if ctx.current_token().kind == tk_seperator:
      ctx.idx += 1
      continue

    let statement = parse_statement(ctx)
    statement.parent = root
    root.children.add(statement)

  return root