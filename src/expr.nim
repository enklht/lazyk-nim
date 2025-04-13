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

const
  S*: uint16 = 0
  K*: uint16 = 1
  I*: uint16 = 2
  V*: uint16 = 3
  Ki*: uint16 = 4
  Inc*: uint16 = 5
  Read*: uint16 = 6

const kComb: uint8 = 0
const kChar: uint8 = 1
const kNum: uint8 = 2

func `$`(self: Atom): string =
  case self.kind
  of kComb:
    case self.val
    of S: "S"
    of K: "K"
    of I: "I"
    of V: "V"
    of Ki: "Ki"
    of Inc: "<inc>"
    of Read: "<read>"
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

func leaf*(item: uint16): Expr =
  Expr(isLeaf: true, item: Atom(kind: kComb, val: item))

func num(n: uint16): Expr =
  Expr(isLeaf: true, item: Atom(kind: kNum, val: n))

func ch(c: uint16): Expr =
  Expr(isLeaf: true, item: Atom(kind: kChar, val: c))

func pair*(left, right: Expr): Expr =
  Expr(isLeaf: false, left: left, right: right)

func pair*(left: Expr, right: uint16): Expr =
  Expr(isLeaf: false, left: left, right: leaf(right))

func pair*(left: uint16, right: Expr): Expr =
  Expr(isLeaf: false, left: leaf(left), right: right)

func pair*(left, right: uint16): Expr =
  Expr(isLeaf: false, left: leaf(left), right: leaf(right))

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
      if top.item.val == I and applicable(1):
        stack[^1] = stack[^1].right
      elif top.item.val == K and applicable(2):
        let x = stack[^1].right
        discard stack.pop()
        stack[^1] = x
      elif top.item.val == KI and applicable(2):
        discard stack.pop()
        let y = stack[^1].right
        stack[^1] = y
      elif top.item.val == S and applicable(3):
        let
          x = stack.pop().right
          y = stack.pop().right
          z = stack[^1].right
        stack[^1].left = pair(x, z)
        stack[^1].right = pair(y, z)
      elif top.item.val == V and applicable(3):
        let
          x = stack.pop().right
          y = stack.pop().right
          f = stack[^1].right
        stack[^1].left = pair(f, x)
        stack[^1].right = y
      elif top.item.val == Inc and applicable(1):
        let n = stack[^1].right.reduce()
        if n.isLeaf and n.item.kind == kNum:
          stack[^1] = num(n.item.val + 1)
        else:
          raise newException(Exception, "cannot increment non number")
      elif top.item.val == Read and applicable(1):
        let c = readChar(stdin)
        let n: uint16 = if c == '\0': 256 else: uint16(c)
        stack[^1].left[] = pair(pair(leaf(V), ch(n)), leaf(Read))[]
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
    var head = pair(pair(pair(current, K), Inc), num(0))
    head = head.reduce()
    if not (head.isLeaf and head.item.kind == kNum):
      raise newException(Exception, &"invalid output format: {head}")
    let n = head.item.val
    if n >= 256:
      return int(n - 256)
    write(stdout, char(n))
    current = pair(current, KI)
