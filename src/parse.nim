import strutils, strformat
import expr

proc parseOne(input: string): (Expr, string)
proc parse*(input: string): Expr

proc parseOne(input: string): (Expr, string) =
  if input.len() == 0:
    raise newException(Exception, "unexpected end of input")

  case input[0]
  of 'S':
    (leaf(S), input[1..^1])
  of 'K':
    (leaf(K), input[1..^1])
  of 'I':
    (leaf(I), input[1..^1])
  of '(':
    let posClosingParen = find(input, ')')
    if posClosingParen == -1:
      raise newException(Exception, "unmatched delimiter")
    let expr = parse(input.substr(1, posClosingParen-1))
    (expr, input.substr(posClosingParen + 1))
  else:
    raise newException(Exception, &"unknown character {input[0]}")

proc parse(input: string): Expr =
  var (current, remaining) = parseOne(input)

  while remaining.len() > 0:
    var next: Expr
    (next, remaining) = parseOne(remaining)
    current = pair(current, next)

  return current
