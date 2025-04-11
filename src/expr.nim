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

func reduce*(self: var Expr)

func spine(self: var Expr, stack: var seq[Expr]) =
  while true:
    stack.add(self)
    if self.isLeaf:
      break
    self = self.left

func reduce(self: var Expr) =
  var stack: seq[Expr] = @[]
  spine(self, stack)

  while true:
    if stack.len() < 1:
      break
    self = stack.pop()

    if self.item.isComb:
      case self.item.comb
      of I:
        if stack.len() < 1:
          break
        self = stack.pop().right
        spine(self, stack)
      of K:
        if stack.len() < 2:
          break
        self = stack.pop().right
        discard stack.pop()
        spine(self, stack)
      of S:
        if stack.len() < 3:
          break
        let
          x = stack.pop().right
          y = stack.pop().right
          z = stack.pop().right
        self = pair(pair(x, z), pair(y, z))
        spine(self, stack)
      of Inc:
        if stack.len() < 1:
          break
        var n = stack.pop().right
        n.reduce()
        if n.isLeaf and n.item.isNum:
          self = num(n.item.num + 1)
        else:
          raise newException(Exception, "cannot increment non number")
      of StdIn:
        raise newException(Exception, "todo")
    else:
      break

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

