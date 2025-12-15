#[
ast_validator.nim
Lammmab - 12/11/25

Walks the AST ensuring-
1. Scoping is handled properly
2. Undeclared variables are not being called
3. All returns and continues are placed in acceptable spots
]#

import ast_defs

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
      echo newPrefix & "└─ Arguments"
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
      echo newPrefix & "├─ Parameters"
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