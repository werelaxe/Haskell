data Tree = Leaf | Node Tree Tree
	deriving(Eq, Read, Show)


createTree :: Tree -> Tree -> Tree
createTree x y = Node x y


simpleTree :: Tree
simpleTree = Node Leaf Leaf


getNodesCount :: Tree -> Int
getNodesCount Leaf = 1
getNodesCount (Node x y) = (getNodesCount x) + (getNodesCount y) + 1


main = do
	let tree = createTree (createTree simpleTree Leaf) simpleTree
	print(getNodesCount tree)

