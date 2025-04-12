type
  Expr* = ref object
    case isLeaf: bool
    of true:
      item: Atom
    of false:
      left, right: Expr
  Atom = object
    case isComb: bool
    of true:
      comb: Combinator
    of false:
      num: int
  Combinator* = enum
    S, K, I,
    Inc = "<inc>", StdIn = "<stdin>"

func `$`(self: Atom): string =
  if self.isComb:
    $self.comb
  else:
    $self.num

func `$`*(self: Expr): string =
  if self.isLeaf:
    result = $self.item
  elif self.right.isLeaf:
    result = $self.left & $self.right
  else:
    result = $self.left & "(" & $self.right & ")"

func isNum(self: Atom): bool {.inline.} =
  not self.isComb

func isCombOf(self: Expr, other: Combinator): bool {.inline.} =
  self.item.comb == other

func `==`(lhs: Atom, rhs: Combinator): bool =
  lhs.isComb and lhs.comb == rhs

func leaf*(item: Combinator): Expr =
  Expr(isLeaf: true, item: Atom(isComb: true, comb: item))

func num(n: int): Expr =
  Expr(isLeaf: true, item: Atom(isComb: false, num: n))

func pair*(left, right: Expr): Expr =
  Expr(isLeaf: false, left: left, right: right)

func pair*(left: Expr, right: Combinator): Expr =
  Expr(isLeaf: false, left: left, right: leaf(right))

func pair*(left: Combinator, right: Expr): Expr =
  Expr(isLeaf: false, left: leaf(left), right: right)

func pair*(left, right: Combinator): Expr =
  Expr(isLeaf: false, left: leaf(left), right: leaf(right))

func reduce*(self: var Expr) =
  var stack: seq[Expr] = @[]
  stack.add(self)

  while true:
    while not stack[^1].isLeaf:
      stack.add(stack[^1].left)

    if stack[^1].item.isComb:
      if stack[^1].isCombOf(I) and stack.len() > 1:
        stack.setLen(stack.len - 1)
        stack[^1] = stack[^1].right
      elif stack[^1].isCombOf(K) and stack.len() > 2:
        let x = stack[^2].right
        stack.setLen(stack.len - 2)
        stack[^1] = x
      elif stack[^1].isCombOf(S) and stack.len() > 3:
        let
          x = stack[^2].right
          y = stack[^3].right
          z = stack[^4].right
        stack.setLen(stack.len - 3)
        stack[^1] = pair(pair(x, z), pair(y, z))
      elif stack[^1].isCombOf(Inc) and stack.len() > 1:
        stack.setLen(stack.len - 1)
        var n = stack[^1].right
        n.reduce()
        if n.isLeaf and n.item.isNum:
          stack[^1] = num(n.item.num + 1)
        else:
          raise newException(Exception, "cannot increment non number")
      elif stack[^1].isCombOf(StdIn):
        raise newException(Exception, "todo")
      else:
        break
    else:
      break
  self = stack[^1]

proc run*(self: var Expr): int =
  self = pair(self, StdIn)
  while true:
    var head = pair(pair(pair(self, K), Inc), num(0))
    head.reduce()
    if not (head.isLeaf and head.item.isNum):
      raise newException(Exception, "invalid output format")
    let n = head.item.num
    if n >= 256:
      return n - 256
    write(stdout, char(n))
    flushFile(stdout)
    self = pair(self, pair(K, I))

