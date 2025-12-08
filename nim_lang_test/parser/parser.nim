import npeg, strutils, tables

type Dict = Table[string, int]


let julianne_parser = peg("program", d: Dict):
  program <- WS * statements * WS
  statements <- line * *(statement_sep * *NL * line )
  line <- EOF | MULTI_LINE_COMMENT | ONE_LINE_COMMENT | statement
  statement <- if_statement | varDecl | var_assign | logical | exp | E"Expected a statement"

  ONE_LINE_COMMENT <- '#' * *(!NL * 1)
  MULTI_LINE_COMMENT <- ";[" * *(!"];" * 1) * "];"
  SKIP <- *(' ' | ONE_LINE_COMMENT | MULTI_LINE_COMMENT)
  statement_sep <- SKIP * sep * SKIP
  sep <- NL | ';'
  NL <- "\n" | "\r\n"
  EOF <- !1
  
  nested_block <- WS * "{" * *NL * WS * statements * WS * *NL * "}" | E"Improper nested block"
  if_statement <- WS * "if" * WS * "(" * WS * statement * WS * ")" * WS * nested_block | E"Improper if"

  exp      <- term * *(WS * >('+' | '-') * WS * term) | E"Expected an expression"
  term     <- factor * *(WS * >('*' | '/') * WS * factor) | E"Expected a term"
  factor   <- '(' * WS * logical * WS * ')' | WS * ident | >boolean | E"Expected number, boolean, or '('"
  ident <- >floating | >integer | >var_ident | E"Expected identifier"
  
  comparison <- exp * *(WS * >("==" | "~=" | "<" | ">" | "<=" | ">=") * WS * exp)
  logical    <- comparison * *(WS * >("&&" | "||") * WS * comparison)
  integer    <- "0" | (nonZeroDigit * *Digit)  
  floating  <- integer * "." * +Digit
  nonZeroDigit <- {'1'..'9'}

  boolean <- ("true" | "false")
  kw <- ("let" | "var")
  varDecl <- (>kw * WS * >var_ident * WS * "=" * WS * logical)
  var_ident <- (+({'a'..'z', '_'}) * *({'a'..'z','A'..'Z','0'..'9','_'}))
  var_assign <- >var_ident * WS * "=" * WS * logical
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
