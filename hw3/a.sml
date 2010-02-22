val x = 5 + 8;
val str = "test";
val str2 = "ing";
val str3 = [1,2,3] @ [4,5,6];
print ((Int.fmt StringCvt.DEC x) ^ "\n");
x;

datatype qtree = Empty | QTree of int * qtree * qtree * qtree * qtree

val qtree1 = QTree(1, Empty, Empty, Empty, Empty);
