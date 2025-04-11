type
  Expr* = ref object
    case isLeaf: bool
    of true:
      item: Combinator  # Leaf node has no data
    of false:
      left, right: Expr
  Combinator* = enum
    S, K, I

func `$`*(tree: Expr): string =
  if tree.isLeaf:
    result = $tree.item
  elif tree.right.isLeaf:
    result = $tree.left & $tree.right
  else:
    result = $tree.left & "(" & $tree.right & ")"

func leaf*(item: Combinator): Expr =
  Expr(isLeaf: true, item: item)

func pair*(left, right: Expr): Expr =
  Expr(isLeaf: false, left: left, right: right)

func pair*(left: Expr, right: Combinator): Expr =
  Expr(isLeaf: false, left: left, right: leaf(right))

func pair*(left: Combinator, right: Expr): Expr =
  Expr(isLeaf: false, left: leaf(left), right: right)

func pair*(left, right: Combinator): Expr =
  Expr(isLeaf: false, left: leaf(left), right: leaf(right))

func reduceOnce(self: var Expr): bool =
  if self.isLeaf:
    return

  if self.left.isLeaf:
    if self.left.item == I:
      self = self.right
      return true
  elif self.left.left.isLeaf:
    if self.left.left.item == K:
      self = self.left.right
      return true
  elif self.left.left.left.isLeaf:
    if self.left.left.left.item == S:
      let
        x = self.left.left.right
        y = self.left.right
        z = self.right
      self = pair(pair(x, z), pair(y, z))
      return true
  return self.left.reduceOnce() or self.right.reduceOnce()

func reduce*(self: var Expr) =
  while self.reduceOnce():
    discard

