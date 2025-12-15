import argparse
import os, sequtils, std/strutils

include parser
include ast
include tokenizer
include ast_validator

proc extract_file_contents(filename: string): string =
  try:
    let inputFile = open(filename, fmRead)
    defer: inputFile.close()
    return inputFile.readAll()
  except:
    echo "Error when opening file ", filename
    quit(1)

proc parse_file(contents: string): seq[string] =
  try:
    return parse(contents)
  except ValueError as e:
    echo "Parse error: ", e.msg
    quit(1)

proc tokenize_file(contents: seq[string]): seq[JulianneToken] =
  try:
    return tokenize(contents)
  except ValueError as e:
    echo "Tokenization error: ", e.msg
    quit(1)

proc transform_ast(tokens: seq[JulianneToken]): ref ASTNode =
  try:
    return ast_transformer(tokens)
  except ValueError as e:
    echo "AST transformation error: ", e.msg
    quit(1)

proc main() =
  var p = newParser:
    arg("filename", help="File to interpret")

    run:
      let filename = opts.filename

      if not endsWith(filename, ".jj"):
        echo "Cannot interpret non-Julianne file; fatal"
        quit(1)

      echo "Processing file: ", filename

      let contents = extract_file_contents(filename)
      let parsed = parse_file(contents)
      echo $parsed

      let tokenized = tokenize_file(parsed)
      for tok in tokenized:
        print_token(tok)

      let root = transform_ast(tokenized)
      printAST(root)

  try:
    p.run()
  except UsageError as e:
    stderr.writeLine getCurrentExceptionMsg()
    quit(1)

main()