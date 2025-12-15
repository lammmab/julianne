type
  TokenKind* = enum
    tk_fl_literal,
    tk_string_raw,
    tk_string_formatted,
    tk_string_normal,
    tk_int_literal,
    tk_null_literal,
    tk_var_identifier,
    tk_const_identifier,
    tk_operator,
    tk_paren_open,
    tk_paren_close,
    tk_brace_open,
    tk_brace_close,
    tk_bracket_open,
    tk_bracket_close,
    tk_comma,
    tk_seperator,
    tk_keyword,
    tk_assign,
    tk_member_access,
    tk_object_identifier,
    tk_type_annotation

  JulianneToken* = ref object
    kind*: TokenKind
    text*: string
