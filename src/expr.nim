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
    Inc = "<inc>", Read = "<read>"

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

var stack*: seq[Expr] = @[]

proc reduce*(self: Expr): Expr =
  stack.add(self)

  while true:
    while not stack[^1].isLeaf:
      stack.add(stack[^1].left)

    let top = stack.pop()

    if not top.item.isComb:
      return top

    if top.isCombOf(I) and stack.len() > 0:
      stack[^1] = stack[^1].right
    elif top.isCombOf(K) and stack.len() > 1:
      let x = stack[^1].right
      discard stack.pop()
      stack[^1] = x
    elif top.isCombOf(S) and stack.len() > 2:
      let
        x = stack.pop().right
        y = stack.pop().right
        z = stack[^1].right
      stack[^1].left = pair(x, z)
      stack[^1].right = pair(y, z)
    elif top.isCombOf(Inc) and stack.len() > 0:
      let n = stack[^1].right.reduce()
      if n.isLeaf and n.item.isNum:
        stack[^1] = num(n.item.num + 1)
      else:
        raise newException(Exception, "cannot increment non number")
    elif top.isCombOf(Read):
      raise newException(Exception, "todo")
    else:
      return top

proc run*(self: Expr): int =
  var current = pair(self, Read)
  while true:
    var head = pair(pair(pair(current, K), Inc), num(0))
    head = head.reduce()
    if not (head.isLeaf and head.item.isNum):
      raise newException(Exception, "invalid output format")
    let n = head.item.num
    if n >= 256:
      return n - 256
    write(stdout, char(n))
    flushFile(stdout)
    current = pair(current, pair(K, I))

