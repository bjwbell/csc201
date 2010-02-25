datatype qtree = Empty | QTree of int * qtree * qtree * qtree * qtree


val qtree1 = QTree(1, Empty, Empty, Empty, Empty);
val qtree2 = QTree(2, Empty, Empty, Empty, Empty);
val qtree3 = QTree(3, Empty, Empty, Empty, Empty);
val qtree4 = QTree(4, Empty, Empty, Empty, Empty);
val level1 = QTree(5, qtree1, Empty, qtree2, Empty);
val level2 = QTree(6, Empty, qtree3, Empty, qtree4);
val level3 = QTree(7, qtree1, qtree2, level1, level2);
val tree = QTree(8, level1, level2, level3, Empty);


fun countqnodes Empty = 0 |
    countqnodes (QTree(_,tree1,tree2,tree3,tree4)) = 1 + countqnodes(tree1) + countqnodes(tree2) + countqnodes(tree3) + countqnodes(tree4)

countqnodes(tree)
