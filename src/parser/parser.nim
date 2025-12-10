import npeg, strutils, tables
import ../ast/ast_remake
import parseutils

type Dict = Table[string, int]

let julianne_parser = peg("program", d: Dict):
  program <- WS * statements * WS
  statements <- line * *(line_sep * line)
  line <- EOF | ONE_LINE_COMMENT | MULTI_LINE_COMMENT | statement
  statement <- >var_declare | >const_declare | >return_stmt | >keyword | >while_loop | >function_declare | >var_assign | >if_statement | >primary_expr | E"Expected a statement"

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
  postfix <- primary * *(call_op | member_op | index_op)
  member_op <- WS * "." * WS * var_ident
  index_op <- WS * "[" * WS * primary_expr * WS * "]" * WS
  call_op <- parentheses_args

  if_template <- parentheses_expr * nested_block
  if_statement <- WS * "if" * if_template * elseif_statement * ?(else_clause)
  elseif_statement <- *(WS * "elseif" * if_template)
  else_clause <- WS * "else" * nested_block
  keyword <- WS * ("continue" | "break") * WS

  return_stmt <- WS * "return" * WS * ?(primary_expr) * WS
  const_declare <- WS * "const" * WS * const_name * WS * "=" * WS * primary_expr_err
  const_name <- +{'A'..'Z'} * *({'A'..'Z'})

  function_declare <- WS * "fn" * WS * var_ident * parentheses_params * nested_block
  parentheses_args <- open_paren * args * close_paren
  args <- ?(WS * primary_expr * WS * *(',' * WS * primary_expr * WS))

  supported_op_symbols <- ("==" | "<=" | ">=" | "~=" | "&" | "+" | "-" | "*" | "/" | "%" | "^" | "~" | "<" | ">" | "|")
  op_symbol <- supported_op_symbols[1..3]
  primary <- parentheses_expr | arr | literal | boolean | var_ident
  product <- postfix
  primary_expr <- WS * product * WS * *(op_symbol * WS * product * WS)
  primary_expr_err <- primary_expr | E"Expected primary expression"
  parentheses_expr <- open_paren * primary_expr * close_paren
  open_paren <- WS * "(" * WS
  close_paren <- WS * ")" * WS

  literal <- floating | integer | str
  str <- normal_str | raw_str | formatted_str

  normal_str <- '"' * (*( escape | (!'"' * 1) )) * '"'
  escape <- '\\' * ('"'|'\\'|'/'|'b'|'f'|'n'|'r'|'t')

  raw_str <- "r" * '"' * (*( '\\' * 1 | (!'"' * 1) )) * '"'
  formatted_str <- "f" * '"' * (*( escape | interpolation | (!'"' * 1) )) * '"'
  interpolation <- '{' * primary_expr_err * '}'

  integer    <- ("0" | (non_zero_digit * *Digit) * !("."))

  floating   <- (*Digit * "." * +Digit)

  non_zero_digit <- {'1'..'9'}

  boolean    <- ("true" | "false")

  kw <- ("let" | "var")
  eq_err <- '=' | E"Malformed declaration"
  var_declare <- (kw * WS * var_ident * WS * eq_err * WS * primary_expr_err)
  var_ident <- (+({'a'..'z', '_'}) * *({'a'..'z','A'..'Z','0'..'9','_'}))
    
  var_assign <- postfix * WS * "=" * WS * primary_expr_err
  params <- ?(WS * var_ident * WS * *(WS * ',' * WS * var_ident * WS))
  parentheses_params <- open_paren * params * close_paren

  WS <- *{' ', '\t'}

proc parse(cnt: string): auto = 
  var d: Dict
  try:
    let res = julianne_parser.match(cnt, d)
    if res.ok:
      return res
    else:
      raise newException(ValueError, "Julianne parsing failed with no exception; fatal")
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
    raise newException(ValueError, "Julianne parsing failed")
