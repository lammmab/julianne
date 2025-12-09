type
  LiteralType = enum
    nkFloat, nkInteger, nkString, nkArr

  AstNodeType = enum
    nkLiteral, nkBinOp, nkIdentifier, nkFunctionCall

  AstNode = ref object of RootObj
    kind: AstNodeType
    line, col: int
    parent: AstNode

  LiteralNode = ref object of AstNode
    case literal_type: LiteralType
    of nkInteger:
        i_v: int
    of  nkFloat:
        f_v: float
    of nkString:
        s_v: string
    of nkArr:
        a_v: seq[AstNode]

  BinaryOpNode = ref object of AstNode
    left, right: AstNode
    op: string

  IdentifierNode = ref object of AstNode
    name: string

template new_literal(T, lit_type: untyped, fieldName: untyped) =
  proc newLit(value: T): LiteralNode =
    let lit = new(LiteralNode)
    lit.kind = nkLiteral
    lit.literal_type = lit_type
    
    lit.fieldName = value 
    
    return lit
    

proc new_binary_op(left: AstNode, right: AstNode, op: string): BinaryOpNode =
  let n = new(BinaryOpNode)
  n.kind = nkBinOp
  n.left = left
  n.right = right
  n.op = op
  left.parent = n
  right.parent = n
  return n

proc new_identifier(name: string): IdentifierNode =
  let n = new(IdentifierNode)
  n.kind = nkIdentifier
  n.name = name
  return n

export newLit