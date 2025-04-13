import strutils, strformat
import expr

proc parseMany(input: string, final: char): (Expr, string)

proc parseOne(input: string): (Expr, string) =
  if input.len() == 0:
    raise newException(Exception, "unexpected end of input")

  case input[0]
  of 'S', 's':
    (S, input[1..^1])
  of 'K', 'k':
    (K, input[1..^1])
  of 'I', 'i':
    (I, input[1..^1])
  of '(':
    parseMany(input[1..^1], ')')
  of ')':
    raise newException(Exception, "unmatched delimiter")
  else:
    raise newException(Exception, &"unknown character: {input[0]}")

proc parseMany(input: string, final: char): (Expr, string) =
  let input = input.strip()
  var (current, remaining) = parseOne(input)

  while remaining.len() > 0:
    remaining = remaining.strip()
    if remaining[0] == final:
      return (current, remaining[1..^1])

    var next: Expr
    (next, remaining) = parseOne(remaining)
    current = pair(current, next)

  return (current, remaining)

proc parse*(input: string): Expr =
  let (current, _) = parseMany(input.strip(), '\0')
  return current
