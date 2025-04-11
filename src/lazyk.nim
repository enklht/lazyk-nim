import expr, parse

when isMainModule:
  var tree = parse("SK(SK)K")
  echo tree
  tree.reduce()
  echo tree

