import strutils
import re

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

const keywords = [
  "let", 
  "var", 
  "const", 
  "fn", 
  "class",
  "if", 
  "else", 
  "elseif", 
  "while", 
  "return",
  "continue",
  "break"
  ]
  
const supported_operators = [
  "==", 
  "<=", 
  ">=", 
  "~=", 
  "&", 
  "+", 
  "-", 
  "*", 
  "/", 
  "%", 
  "^", 
  "~", 
  "<", 
  ">", 
  "|"
  ]

proc is_operator(text: string): bool =
  if text.len < 1 or text.len > 3:
    return false
  result = text in supported_operators

proc is_null(text: string): bool =
  case text
  of "null":
    return true
  of "nil":
    return true
  of "none":
    return true
  else:
    return false

proc is_keyword(s: string): bool =
  s in keywords

proc is_int(s: string): bool =
  try:
    discard parseInt(s)
    true
  except:
    false

proc classify_string_literal(s: string): TokenKind =
  if s.startsWith("r\""):
    return tk_string_raw
  elif s.startsWith("f\""):
    return tk_string_formatted
  elif s.startsWith("\""):
    return tk_string_normal
  else:
    raise newException(ValueError, "Bad string literal: " & s)

proc is_float(s: string): bool =
  try:
    discard parseFloat(s)
    true
  except:
    false

proc is_var_identifier(s: string): bool =
  if s.len == 0:
    return false
  if not (s[0] in 'a'..'z' or s[0] == '_'):
    return false
  for c in s[1..^1]:
    if not (c in 'a'..'z' or c in 'A'..'Z' or c in '0'..'9' or c == '_'):
      return false
  return true

proc is_const_identifier(s: string): bool =
  if s.len == 0:
    return false
  for c in s:
    if not (c in 'A'..'Z'):
      return false
  return true

proc is_object_identifier(s: string): bool =
  s.match(re"^[A-Z]+[A-Za-z]+$")


proc print_token*(t: JulianneToken) =
  echo "Token(kind=$1,text=$2)" % [$t.kind,escape(t.text)]

proc new_token*(kind: TokenKind, text: string): JulianneToken =
  let token: JulianneToken = new JulianneToken
  token.kind = kind
  token.text = text
  token

proc match_symbol(text: string): TokenKind =
  case text
  of "=":
    return tk_assign
  of ".":
    return tk_member_access
  of "{":
    return tk_brace_open
  of "}":
    return tk_brace_close
  of "(":
    return tk_paren_open
  of ")":
    return tk_paren_close
  of "[":
    return tk_bracket_open
  of "]":
    return tk_bracket_close
  of ",":
    return tk_comma
  of ";":
    return tk_seperator
  of "::":
    return tk_type_annotation
  of "\n":
    return tk_seperator
  else:
    if is_operator(text):
      return tk_operator
    else:
      raise newException(ValueError, "Unknown operator: " & text)

proc match_str_to_token(text: string): TokenKind =
  try:
    return classify_string_literal(text)
  except:
    discard
  let normalized: string = text.replace("\r\n","\n")

  if is_keyword(normalized):
    return tk_keyword
  elif is_int(normalized):
    return tk_int_literal
  elif is_float(normalized):
    return tk_fl_literal
  elif is_null(normalized):
    return tk_null_literal
  elif is_const_identifier(normalized):
    return tk_const_identifier
  elif is_var_identifier(normalized):
    return tk_var_identifier
  elif is_object_identifier(normalized):
    return tk_object_identifier
  else:
    return match_symbol(normalized)

proc tokenize(l: seq[string]): seq[JulianneToken] =
  var final: seq[JulianneToken] = @[]
  for text in l:
    let kind: TokenKind = match_str_to_token(text)
    final.add(new_token(kind,text))
  return final