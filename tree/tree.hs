data Tree = Leaf | Node Tree Tree
	deriving(Eq, Read, Show)


createTree :: Tree -> Tree -> Tree
createTree x y = Node x y


simpleTree :: Tree
simpleTree = Node Leaf Leaf


getLeavesCount :: Tree -> Int
getLeavesCount Leaf = 1
getLeavesCount (Node x y) = (getLeavesCount x) + (getLeavesCount y)


getPathSum :: Tree -> Int
getPathSum Leaf = 0
getPathSum (Node x y) = (getLeavesCount x) + (getPathSum x) + (getLeavesCount y) + (getPathSum y)


main = do
	let tree = createTree simpleTree (createTree simpleTree Leaf)
	print(getPathSum tree)
