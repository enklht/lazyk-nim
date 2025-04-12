import std/syncio, std/parseopt
import expr, parse

when isMainModule:
  var fname = ""
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      fname = key
    of cmdLongOption, cmdShortOption:
      case key
      of "h":
        echo "todo: help"
        quit 0
      of "v":
        echo "todo: version"
        quit 0
      else:
        echo "unknown options"
    of cmdEnd:
      discard

  if fname == "":
    echo "fname not provided"
    quit 0
  
  setStdIoUnbuffered()
  var source = readFile(fname)
  var input = parse(source)
  quit run input
