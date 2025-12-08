import npeg, strutils, tables

type Dict = Table[string, int]


let julianne_parser = peg("program", d: Dict):
  program <- WS * (statement * *(WS * sep * *NL * WS * statement)) * WS
  statement <- varDecl | var_assign | exp | E"Expected a statement"

  sep <- NL | ';'
  NL <- "\n" | "\r\n"
  exp      <- term * *(WS * >('+' | '-') * WS * term) | E"Expected an expression"
  term     <- factor * *(WS * >('*' | '/') * WS * factor) | E"Expected a term"
  factor   <- WS * ident | '(' * WS * exp * WS * ')' | E"Expected number or '('"
  ident <- floating | integer | var_ident | E"Expected identifier"
  integer    <- "0" | (nonZeroDigit * *Digit) 
  floating  <- integer * "." * +Digit
  nonZeroDigit <- {'1'..'9'}

  boolean <- >("true" | "false")
  kw <- >("let" | "var")
  varDecl <- (kw * WS * var_ident * WS * "=" * WS * >exp)
  var_ident <- >(+({'a'..'z', '_'}) * *({'a'..'z','A'..'Z','0'..'9','_'}))
  var_assign <- var_ident * WS * "=" * WS * exp
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
