import strformat

type
  Expr* = ref object
    case isLeaf: bool
    of true:
      item: Atom
    of false:
      left, right: Expr
  Atom = object
    kind: uint8
    val: uint16

const kComb: uint8 = 0
const kChar: uint8 = 1
const kNum: uint8 = 2

template defComb(name: untyped, v: uint16) =
  const `val name` {.inject.} = v
  let `name`* {.inject.} = Expr(isLeaf: true, item: Atom(kind: kComb, val: v))

defComb(S, 0)
defComb(K, 1)
defComb(I, 2)
defComb(V, 3)
defComb(Ki, 4)
defComb(Inc, 5)
defComb(Read, 6)

template numExpr(n: uint16): Expr =
  Expr(isLeaf: true, item: Atom(kind: kNum, val: n))

template charExpr(c: uint16): Expr =
  Expr(isLeaf: true, item: Atom(kind: kChar, val: c))

func pair*(left, right: Expr): Expr {.inline.} =
  Expr(isLeaf: false, left: left, right: right)

func `$`(self: Atom): string =
  case self.kind
  of kComb:
    case self.val
    of valS: "S"
    of valK: "K"
    of valI: "I"
    of valV: "V"
    of valKi: "Ki"
    of valInc: "<inc>"
    of valRead: "<read>"
    else: raise newException(Exception, "unreachable")
  of kNum:
    $self.val
  of kChar:
    $char(self.val)
  else: raise newException(Exception, "unreachable")

func `$`*(self: Expr): string =
  if self.isLeaf:
    result = $self.item
  elif self.right.isLeaf:
    result = $self.left & $self.right
  else:
    result = $self.left & "(" & $self.right & ")"

var stack: seq[Expr] = @[]

proc reduce*(self: Expr): Expr =
  let bottom = stack.len()
  template applicable(n: int): bool = stack.len() - bottom >= n

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
        stack[^1].left = I
        stack[^1].right = x
        stack[^1] = x
      elif top == KI and applicable(2):
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
        let n: uint16 = if c == '\0': 256 else: uint16(c)
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
      raise newException(Exception, &"invalid output format: {head}")
    let n = head.item.val
    if n >= 256:
      return int(n - 256)
    write(stdout, char(n))
    current = pair(current, KI)
