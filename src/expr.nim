type
  Expr* {.acyclic.} = ref object
    case isLeaf: bool
    of true:
      item: Atom
    of false:
      left, right: Expr

  AtomKind = enum
    kComb
    kChar
    kNum

  Atom = object
    kind: AtomKind
    val: uint

let S* = Expr(isLeaf: true, item: Atom(kind: kComb))
let K* = Expr(isLeaf: true, item: Atom(kind: kComb))
let I* = Expr(isLeaf: true, item: Atom(kind: kComb))
let V = Expr(isLeaf: true, item: Atom(kind: kComb))
let Ki = Expr(isLeaf: true, item: Atom(kind: kComb))
let Inc = Expr(isLeaf: true, item: Atom(kind: kComb))
let Read = Expr(isLeaf: true, item: Atom(kind: kComb))

template numExpr(n: uint): Expr =
  Expr(isLeaf: true, item: Atom(kind: kNum, val: n))

template charExpr(c: uint): Expr =
  Expr(isLeaf: true, item: Atom(kind: kChar, val: c))

func pair*(left, right: Expr): Expr {.inline.} =
  Expr(isLeaf: false, left: left, right: right)

when defined(debug):
  func `$`(self: Atom): string =
    case self.kind
    of kComb:
      case self
      of S: "S"
      of K: "K"
      of I: "I"
      of V: "V"
      of Ki: "Ki"
      of Inc: "<inc>"
      of Read: "<read>"
      else: raise newException(Exception, "unreachable")
    of kNum: $self.val
    of kChar: $char(self.val)

  func `$`*(self: Expr): string =
    if self.isLeaf:
      result = $self.item
    elif self.right.isLeaf:
      result = $self.left & $self.right
    else:
      result = $self.left & "(" & $self.right & ")"

var stack = newSeq[Expr]()

proc reduce*(self: Expr): Expr =
  let bottom = stack.len()
  template applicable(n: int): bool =
    stack.len() - bottom >= n

  var top: Expr

  stack.add(self)

  while true:
    while not stack[^1].isLeaf:
      stack.add(stack[^1].left)

    top = stack.pop()

    case top.item.kind
    of kComb:
      if top == I and applicable(1):
        stack[^1] = stack[^1].right
      elif top == K and applicable(2):
        let x = stack[^1].right
        discard stack.pop()
        stack[^1] = x
      elif top == Ki and applicable(2):
        discard stack.pop()
        let y = stack[^1].right
        stack[^1] = y
      elif top == S and applicable(3):
        let
          x = stack.pop().right
          y = stack.pop().right
          z = stack[^1].right
        stack[^1].left = pair(x, z)
        stack[^1].right = pair(y, z)
      elif top == V and applicable(3):
        let
          x = stack.pop().right
          y = stack.pop().right
          f = stack[^1].right
        stack[^1].left = pair(f, x)
        stack[^1].right = y
      elif top == Inc and applicable(1):
        let n = stack[^1].right.reduce()
        if n.isLeaf and n.item.kind == kNum:
          stack[^1][] = numExpr(n.item.val + 1)[]
        else:
          raise newException(Exception, "cannot increment non number")
      elif top == Read and applicable(1):
        let c = readChar(stdin)
        let n: uint =
          if c == '\0': 256 else: uint(c)
        stack[^1].left[] = pair(pair(V, charExpr(n)), Read)[]
      else:
        break
    of kChar:
      if applicable(2):
        let n = top.item.val
        if n == 0:
          discard stack.pop()
          let x = stack[^1].right
          stack[^1] = x
        else:
          top.item.val -= 1
          let
            f = stack.pop().right
            nfx = stack.pop()
          stack.add(pair(f, nfx))
    else:
      break

  while stack.len() > bottom:
    let parent = stack.pop()
    parent.left = top
    top = parent
  return top

proc run*(self: Expr): int =
  var current = pair(self, Read)
  while true:
    var head = pair(pair(pair(current, K), Inc), numExpr(0))
    head = head.reduce()
    if not (head.isLeaf and head.item.kind == kNum):
      raise newException(Exception, "invalid output format")
    let n = head.item.val
    if n >= 256:
      return int(n - 256)
    write(stdout, char(n))
    current = pair(current, Ki)
