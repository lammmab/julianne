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

  ObjectDeclaration* = object
    identifier: string
    fields: seq[ref ASTNode]
    methods: seq[ref ASTNode]

  MemberAccess* = object
    target: ref ASTNode
    property: ref ASTNode

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
