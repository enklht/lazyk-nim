import strutils, strformat
import expr

func parseMany(input: string, final: char): (Expr, string)

func parseOne(input: string): (Expr, string) =
  if input.len() == 0:
    raise newException(Exception, "unexpected end of input")

  case input[0]
  of 'S', 's':
    (leaf(S), input[1..^1])
  of 'K', 'k':
    (leaf(K), input[1..^1])
  of 'I', 'i':
    (leaf(I), input[1..^1])
  of '(':
    parseMany(input[1..^1], ')')
  of ' ', '\t', '\n':
    parseOne(input[1..^1])
  else:
    raise newException(Exception, &"unknown character {input}")

func parseMany(input: string, final: char): (Expr, string) =
  var (current, remaining) = parseOne(input)

  while remaining.len() > 0:
    if remaining[0] == final:
      return (current, remaining[1..^1])

    var next: Expr
    (next, remaining) = parseOne(remaining)
    current = pair(current, next)

  return (current, remaining)

func parse*(input: string): Expr =
  let (current, remaining) = parseMany(input.strip(), '\0')
  return current
