data Tree = Leaf | Node Tree Tree
	deriving(Eq, Read, Show)


createTree :: Tree -> Tree -> Tree
createTree x y = Node x y


simpleTree :: Tree
simpleTree = Node Leaf Leaf


getLeavesCount :: Tree -> Int
getLeavesCount Leaf = 1
getLeavesCount (Node x y) = (getLeavesCount x) + (getLeavesCount y)


getPathSumAccumulate :: Tree -> Int -> Int
getPathSumAccumulate Leaf _ = 0
getPathSumAccumulate (Node Leaf Leaf) _ = 2
getPathSumAccumulate (Node Leaf y) collected = 1 + (getLeavesCount y) + (getPathSumAccumulate y collected)
getPathSumAccumulate (Node x Leaf) collected = (getLeavesCount x) + (getPathSumAccumulate x collected) + 1
getPathSumAccumulate (Node x y) collected = (getLeavesCount x) + (getPathSumAccumulate x collected) + (getLeavesCount y) + (getPathSumAccumulate y collected)


getPathSum :: Tree -> Int
getPathSum tree = getPathSumAccumulate tree 0

main = do
	let tree = createTree simpleTree (createTree simpleTree Leaf)
	print(getPathSum tree)
