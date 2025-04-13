import strformat, std/syncio, parseopt
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
        echo "Usage: ./lazyk [options] sourcefile"
        echo "  -h    print this help and exit"
        echo "  -u    disable stdout buffering"
        echo "  -v    print version and exit"
        quit 0
      of "v":
        echo "Lazy K interpreter 0.1.0 by enklht"
        quit 0
      of "u":
        setStdIoUnbuffered()
      else:
        echo &"bad option {key} (Try -h for more information)"
    of cmdEnd:
      discard

  if fname == "":
    echo "fname not provided"
    quit 0

  var source = readFile(fname)
  var input = parse(source)
  quit run(input)
