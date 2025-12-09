import npeg, strutils, tables
import ../ast/ast
import parseutils

type Dict = Table[string, int]

proc collapse_bin_expr(ops: seq[string], children: seq[(string, ExprNode)]) =
  var left = cast[ExprNode](pop_node())
  for (op, rightNode) in children:
    let right = cast[ExprNode](rightNode)
    left = new_binary_expr(left, op, right)
  push_node(left)

let julianne_parser = peg("program", d: Dict):
  program <- >(WS * statements * WS):
    dump_ast_stack()
  statements <- line * *(line_sep * line)
  line <- EOF | ONE_LINE_COMMENT | MULTI_LINE_COMMENT | statement
  statement <- var_declare | const_declare | return_stmt | keyword | while_loop | function_declare | var_assign | primary_expr | E"Expected a statement"

  ONE_LINE_COMMENT <- '#' * *(!NL * 1)
  MULTI_LINE_COMMENT <- ";[" * *(!"];" * 1) * "];"
  SKIP <- *(' ' | ONE_LINE_COMMENT | MULTI_LINE_COMMENT)
  line_sep <- sep * *NL
  sep <- SKIP * (NL | ';') * SKIP
  NL <- "\n" | "\r\n"
  EOF <- !1
  EMPTY_LINE <- WS * SKIP * WS

  obj_ident <- WS * "::" * WS * obj_name
  obj_name <- +{'A'..'Z'} * +({'A'..'Z'} | {'a'..'z'})

  nested_sep <- WS * (sep) * WS
  nested_block <- WS * "{" * WS * *(*NL * WS * ((!"}" * statement) * nested_sep) * WS) * *NL * WS * "}" | E"Malformed nested block"

  arr <- "[" * WS * ?(primary_expr * *(WS * "," * WS * primary_expr)) * WS * "]"

  while_loop <- WS * "while" * ?parentheses_expr * nested_block
  postfix <- primary * >*(call_op | member_op | index_op)
  member_op <- WS * "." * WS * var_ident
  index_op <- WS * "[" * WS * primary_expr * WS * "]" * WS
  call_op <- parentheses_args

  if_template <- parentheses_expr * nested_block
  if_statement <- WS * "if" * if_template * elseif_statement * ?(else_clause)
  elseif_statement <- *(WS * "elseif" * if_template)
  else_clause <- WS * "else" * nested_block
  keyword <- WS * ("continue" | "break") * WS

  return_stmt <- WS * "return" * WS * >?(primary_expr) * WS
  const_declare <- WS * "const" * WS * >const_name * WS * "=" * WS * >primary_expr_err
  const_name <- +{'A'..'Z'} * *({'A'..'Z'})

  function_declare <- WS * "fn" * WS * var_ident * parentheses_params * nested_block
  parentheses_args <- open_paren * args * close_paren
  args <- ?(WS * primary_expr * WS * *(',' * WS * primary_expr * WS))

  primary <- parentheses_expr | arr | literal | boolean | var_ident
  product <- postfix
  sum <- product * (*(WS * >("*" | "/") * WS * product)):
    discard
    #echo $1
    #collapse_bin_expr()
  comparison <- sum * (*(WS * ("+" | "-") * WS * sum))
  factor <- comparison * (*(WS * ("==" | "~=" | "<" | ">" | "<=" | ">=") * WS * comparison))
  term <- factor * (*(WS * "&&" * WS * factor))
  primary_expr <- term * (*(WS * "||" * WS * term))
  primary_expr_err <- primary_expr | E"Expected primary expression"
  parentheses_expr <- open_paren * primary_expr * close_paren
  open_paren <- WS * "(" * WS
  close_paren <- WS * ")" * WS

  literal <- floating | integer | str
  str <- >normal_str | >raw_str | >formatted_str:
    let str = $1;
    push_str_expr(str);

  normal_str <- '"' * (*( escape | (!'"' * 1) )) * '"'
  escape <- '\\' * ('"'|'\\'|'/'|'b'|'f'|'n'|'r'|'t')

  raw_str <- "r" * '"' * (*( '\\' * 1 | (!'"' * 1) )) * '"'
  formatted_str <- "f" * '"' * (*( escape | interpolation | (!'"' * 1) )) * '"'
  interpolation <- '{' * primary_expr_err * '}'

  integer    <- >("0" | (non_zero_digit * *Digit)) * !("."):
    let i = $1;
    echo "Capturing integer";
    push_int_expr(parseInt(i));

  floating   <- >(*Digit * "." * +Digit):
    let i = $1;
    echo "Capturing float";
    push_float_expr(parseFloat(i));

  non_zero_digit <- {'1'..'9'}

  boolean    <- >("true" | "false"):
    let i = $1;
    echo "Capturing boolean";
    push_bool_expr(i == "true");

  kw <- ("let" | "var")
  var_declare <- (kw * WS * var_ident * WS * "=" * WS * primary_expr_err):
    let valNode = cast[ExprNode](pop_node())
    let nameNode = cast[ExprNode](pop_node())
    echo "Capturing variable declaration"
    push_node(new_var_decl(nameNode.id.name, valNode));

  var_ident <- >(+({'a'..'z', '_'}) * *({'a'..'z','A'..'Z','0'..'9','_'})):
    let i = $1;
    echo "Capturing identifier"
    push_identifier_expr(i);
    
  var_assign <- postfix * WS * "=" * WS * primary_expr_err:
    let new_val = cast[ExprNode](pop_node())
    let old_ident = cast[ExprNode](pop_node())
    echo "Capturing variable assignment"
    push_node(new_assign(old_ident,new_val))

  params <- ?(WS * var_ident * WS * *(WS * ',' * WS * var_ident * WS))
  parentheses_params <- open_paren * params * close_paren

  WS <- *{' ', '\t'}

proc parse(cnt: string) = 
  var d: Dict
  try:
    let result = julianne_parser.match(cnt, d)
    echo result
    if result.ok:
      echo "Parse OK"
    else:
      echo "Parse failed (no exception)"
  except NPegParseError as e:
    let pos = e.matchMax
    var lineNum = 1
    var colNum = 1
    for i, c in cnt:
      if i >= pos:
        break
      if c == '\n':
        lineNum += 1
        colNum = 1
      else:
        colNum += 1

    echo "Julianne parsing failed with error: ", e.msg
    echo "At line ", lineNum, ", column ", colNum
