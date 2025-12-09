include parser/parser
import argparse
import os, sequtils

proc extract_file_contents(filename: string): string =
  let inputFile = open(filename, fmRead)
  defer: inputFile.close()
  return inputFile.readAll()

proc main() =
  var p = newParser:  
    arg("filename", help="File to interpret")
    
    run:
      let filename = opts.filename
      echo "Processing file: ", filename
      parse(extract_file_contents(filename))

  try:
    p.run()
  except UsageError as e:
    stderr.writeLine getCurrentExceptionMsg()
    quit(1)

main()