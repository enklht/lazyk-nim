type
  Expr* = ref object
    case isLeaf: bool
    of true:
      item: Atom
    of false:
      left, right: Expr
  AtomKind = enum
    kComb, kNum, kChar
  Atom = object
    case kind: AtomKind
    of kComb:
      comb: Combinator
    of kNum:
      num: uint16
    of kChar:
      char: uint16
  Combinator* = enum
    S, K, I, V, KI = "Ki",
    Inc = "<inc>", Read = "<read>",

func `$`(self: Atom): string =
  case self.kind
  of kComb:
    $self.comb
  of kNum:
    $self.num
  of kChar:
    $self.char

func `$`*(self: Expr): string =
  if self.isLeaf:
    result = $self.item
  elif self.right.isLeaf:
    result = $self.left & $self.right
  else:
    result = $self.left & "(" & $self.right & ")"

func leaf*(item: Combinator): Expr =
  Expr(isLeaf: true, item: Atom(kind: kComb, comb: item))

func num(n: uint16): Expr =
  Expr(isLeaf: true, item: Atom(kind: kNum, num: n))

func ch(c: uint16): Expr =
  Expr(isLeaf: true, item: Atom(kind: kChar, char: c))

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
  let stackLen = stack.len()
  var top: Expr

  stack.add(self)

  while true:
    while not stack[^1].isLeaf:
      stack.add(stack[^1].left)

    top = stack.pop()

    case top.item.kind
    of kComb:
      if top.item.comb == I and stack.len() > 0:
        stack[^1] = stack[^1].right
      elif top.item.comb == K and stack.len() > 1:
        let x = stack[^1].right
        discard stack.pop()
        stack[^1] = x
      elif top.item.comb == KI and stack.len() > 1:
        discard stack.pop()
        let y = stack[^1].right
        stack[^1] = y
      elif top.item.comb == S and stack.len() > 2:
        let
          x = stack.pop().right
          y = stack.pop().right
          z = stack[^1].right
        stack[^1].left = pair(x, z)
        stack[^1].right = pair(y, z)
      elif top.item.comb == V and stack.len() > 2:
        let
          x = stack.pop().right
          y = stack.pop().right
          f = stack[^1].right
        stack[^1].left = pair(f, x)
        stack[^1].right = y
      elif top.item.comb == Inc and stack.len() > 0:
        let n = stack[^1].right.reduce()
        if n.isLeaf and n.item.kind == kNum:
          stack[^1] = num(n.item.num + 1)
        else:
          raise newException(Exception, "cannot increment non number")
      elif top.item.comb == Read and stack.len() > 1:
        let c = readChar(stdin)
        let n: uint16 = if c == '\0': 256 else: uint16(c)
        stack[^1].left[] = pair(pair(leaf(V), ch(n)), leaf(Read))[]
      else:
        break
    of kNum:
      break
    of kChar:
      if stack.len() > 1:
        let n = top.item.char
        if n == 0:
          discard stack.pop()
          let x = stack[^1].right
          stack[^1] = x
        else:
          top.item.char -= 1
          let
            f = stack.pop().right
            nfx = stack.pop()
          stack.add(pair(f, nfx))

  while stack.len() > stackLen:
    let parent = stack.pop()
    parent.left = top
    top = parent
  return top

proc run*(self: Expr): int =
  var current = pair(self, Read)
  while true:
    var head = pair(pair(pair(current, K), Inc), num(0))
    head = head.reduce()
    if not (head.isLeaf and head.item.kind == kNum):
      raise newException(Exception, "invalid output format")
    let n = head.item.num
    if n >= 256:
      return int(n - 256)
    write(stdout, char(n))
    current = pair(current, KI)

