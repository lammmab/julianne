include parser
include tokenizer
import argparse
import os, sequtils, std/strutils
include ast_validator

proc extract_file_contents(filename: string): string =
  try:
    let inputFile = open(filename, fmRead)
    defer: inputFile.close()
    return inputFile.readAll()
  except:
    echo "Error when opening file ", filename
    quit(1)

proc main() =
  var p = newParser:  
    arg("filename", help="File to interpret")
    
    run:
      let filename = opts.filename
      
      if not endsWith(filename,".jj"):
        echo "Cannot interpret non-Julianne file; fatal"
        quit(1)
      echo "Processing file: ", filename
      try:
        let contents: seq[string] = parse(extract_file_contents(filename))
        echo $contents
        var tokenized: seq[JulianneToken]
        try:
          tokenized = tokenize(contents)
        except ValueError as e:
          echo e.msg
          quit(1)
        for tok in tokenized:
          print_token(tok)
        try:
          let root: ref ASTNode = ast_transformer(tokenized)
          printAST(root)
        except ValueError as e:
          echo e.msg
          quit(1)
      except:
        quit(1)
  try:
    p.run()
  except UsageError as e:
    stderr.writeLine getCurrentExceptionMsg()
    quit(1)

main()