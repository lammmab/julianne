type
  TokenKind* = enum
    tk_literal,
    tk_identifier,
    tk_operator,
    tk_paren_open,
    tk_paren_close,
    tk_comma

  Token* = object
    kind*: TokenKind
    text*: string