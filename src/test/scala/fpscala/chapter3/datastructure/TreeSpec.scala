package fpscala.chapter3.datastructure

import org.scalatest.FlatSpec

class TreeSpec extends FlatSpec {


  "Tree" should "Exercise 3.25: Write a function size that counts the number of nodes (leaves and branches) " +
    "in a tree. " in {
    """
      |   A
      | B   C
      |    D E
      |
    """.stripMargin


    val tree = Branch(Leaf("B"), Branch(Leaf("D"), Leaf("E")))

    assert(Tree.size(Leaf("A")) == 1)
    assert(Tree.size(tree) == 5)
    assert(Tree.size(Branch(Leaf("A"), Leaf("B"))) == 3)

  }

  it should "Exercise 3.26: Write a function maximum that returns the maximum element in a Tree[Int]" in {
    assert(Tree.maximum(Leaf(1)) == 1)
    assert(Tree.maximum(Branch(Leaf(1), Branch(Leaf(5), Leaf(3)))) == 5)
    assert(Tree.maximum(Branch(Leaf(-1), Branch(Leaf(-5), Leaf(-3)))) == -1)
    assert(Tree.maximum(Branch(Leaf(1), Branch(Branch(Leaf(6), Leaf(9)), Branch(Leaf(8), Leaf(-1))))) == 9)
  }

  it should "Exercise 3.27: Write a function depth that returns the maximum path length from the root of" +
    " a tree to any leaf." in {
    assert(Tree.depth(Leaf(1)) == 0)
    assert(Tree.depth(Branch(Leaf("B"), Branch(Leaf("D"), Leaf("E")))) == 2)
  }

  it should "Exercise 3.28: Write a function map, analogous to the method of the same name on List, " +
    "that modifies each element in a tree with a given function. " in {
    val mapToString = (x: Int) => x.toString
    val treeInt = Branch(Leaf(1), Branch(Leaf(5), Leaf(3)))
    val treeString = Branch(Leaf("1"), Branch(Leaf("5"), Leaf("3")))
    assert(Tree.map(treeInt)(mapToString) == treeString)
    assert(Tree.map(Leaf(1))(mapToString) == Leaf("1"))
  }

}
