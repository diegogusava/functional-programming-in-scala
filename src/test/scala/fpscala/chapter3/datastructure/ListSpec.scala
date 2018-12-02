package fpscala.chapter3.datastructure

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

  "A List" should "be created" in {
    assert(List(1, 2, 3, 4) == Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
  }

  it should "create an appropriate Cons object" in {
    val list = List(1, 2).asInstanceOf[Cons[Int]]
    assert(list.head == 1)
    assert(list.tail == Cons(2, Nil))
  }

  it should "return the head" in {
    assert(List.getHead(List(1, 2, 3, 4)) == 1)
  }

  it should "return the tail" in {
    assert(List.getTail(List(1, 2, 3, 4)) == List(2, 3, 4))
  }

  it should "set the head element" in {
    assert(List.setHead(10, List(1, 2, 3, 4)) == List(10, 2, 3, 4))
  }

  it should "drop 2 elements and return the tail" in {
    assert(List.drop(List(1, 2, 3, 4), 2) == List(3, 4))
  }

  it should "append two list" in {
    assert(List.append(List(1), List(2)) == List(1, 2))
  }

  it should "throw an error if drop on empty list" in {
    assertThrows[RuntimeException] {
      List.drop(Nil, 2)
    }
  }

  it should "drop while the predicate is true" in {
    val result = List.dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3)
    assert(result == List(3, 4))
  }

  it should "drop while the predicate is true and return an empty list" in {
    val result = List.dropWhile(List(1, 2, 3, 4), (x: Int) => x < 5)
    assert(result == Nil)
  }

  it should "return the list without the last element" in {
    val result = List.init(List(1, 2, 3, 4))
    assert(result == List(1, 2, 3))
  }

  it should "fold to right and map each element to string" in {
    val acc = List[String]()
    val list = List(1, 2, 3, 4)
    val mapToString = (curr: Int, acc: List[String]) => Cons(curr.toString, acc)
    val result = List.foldRight(list, acc)(mapToString)
    assert(result == List("1", "2", "3", "4"))
  }

  it should "Exercise 3.9: Compute the length of a list using foldRight" in {
    assert(List.length(List(1, 2, 3, 4)) == 4)
    assert(List.length(List()) == 0)
  }

  it should " Exercise 3.10: foldLeft that is tail-recursive" in {
    val orderOfFoldLeft = (acc: List[Int], curr: Int) => Cons(curr, acc)
    assert(List.foldLeft(List(), List[Int]())(orderOfFoldLeft) == List())
    assert(List.foldLeft(List(1, 2, 3, 4), List[Int]())(orderOfFoldLeft) == List(4, 3, 2, 1))
  }

  it should "Exercise 3.12: Write a function that returns the reverse of a list (given List(1,2,3) it returns " +
    "List(3,2,1)). See if you can write it using a fold." in {
    assert(List.reverse(List()) == List())
    assert(List.reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
    assert(List.reverse(List("Diego", "Armando", "Gusava")) == List("Gusava", "Armando", "Diego"))
  }

  it should "Exercise 3.13: foldLeft in terms of foldRight" in {
    val orderOfFoldLeft = (acc: List[Int], curr: Int) => Cons(curr, acc)
    assert(List.foldLeftUsingFoldRight(List(), List[Int]())(orderOfFoldLeft) == List())
    assert(List.foldLeftUsingFoldRight(List(1, 2, 3, 4), List[Int]())(orderOfFoldLeft) == List(4, 3, 2, 1))
  }

  it should "Exercise 3.13: fold to right using fold left" in {
    val acc = List[String]()
    val list = List(1, 2, 3, 4)
    val mapToString = (curr: Int, acc: List[String]) => Cons(curr.toString, acc)
    val result = List.foldRightUsingFoldLeft(list, acc)(mapToString)
    assert(result == List("1", "2", "3", "4"))
  }

  it should "Exercise 3.14: append using fold left" in {
    assert(List.appendFoldLeft(List(1), List(2)) == List(1, 2))
    assert(List.appendFoldLeft(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  it should "Exercise 3.14: append using fold right" in {
    assert(List.appendFoldRight(List(1), List(2)) == List(1, 2))
    assert(List.appendFoldRight(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  it should "Exercise 3.15: Write a function that concatenates a list of lists into a single list. Its runtime " +
    "should be linear in the total length of all lists." in {
    assert(List.flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
    assert(List.flatten(List(List(1, 2, List(3, 4)), List(5, 6))) == List(1, 2, List(3, 4), 5, 6))

    assert(List.flatten2(List(List(1, 2), List(3, 4), 5)) == List(1, 2, 3, 4, 5))
  }

  it should "Exercise 3.16: Write a function that transforms a list of integers by adding 1 to each element. " in {
    assert(List.addOne(List(1, 2, 3, 4)) == List(2, 3, 4, 5))
  }

  it should "Exercise 3.17: Write a function that turns each value in a List[Double] into a String." in {
    assert(List.eachElementToString(List(1, 2, 3, 4)) == List("1", "2", "3", "4"))
  }

  it should "Exercise 3.18: Write a function map that generalizes modifying each element in a list while maintaining" +
    " the structure of the list" in {
    val result = List.map(List(1, 2, 3, 4))((curr) => curr.toString)
    assert(result == List("1", "2", "3", "4"))
  }

  it should "Exercise 3.19: Write a function filter that removes elements from a list unless they satisfy a given" +
    " predicate. Use it to remove all odd numbers from a List[Int]." in {
    val result = List.filter(List(1, 2, 3, 4))((curr) => curr % 2 == 0)
    assert(result == List(2, 4))
  }

  it should "Exercise 3.20: Write a function flatMap that works like map except that the function given will return" +
    " a list instead of a single result, and that list should be inserted into the final resulting list." in {
    val result = List.flatMap(List(1, 2, 3))(i => List(i, i))
    assert(result == List(1, 1, 2, 2, 3, 3))
  }

  it should "Exercise 3.21: Use flatMap to implement filter" in {
    val result = List.filterFlatMap(List(1, 2, 3, 4))((curr) => curr % 2 == 0)
    assert(result == List(2, 4))
  }

  it should "Exercise 3.22: Write a function that accepts two lists and constructs a new list by adding corresponding " +
    "elements." in {
    //List(1,2,3) and List(4,5,6) become List(5,7,9).

    assert(List.addElements(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(List.addElements(List(1, 2), List(4, 5, 6)) == List(5, 7, 6))
    assert(List.addElements(List(1, 2, 3), List(4, 5)) == List(5, 7, 3))

    assert(List.addElementsToLeft(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(List.addElementsToLeft(List(1, 2), List(4, 5, 6)) == List(5, 7, 6))
    assert(List.addElementsToLeft(List(1, 2, 3), List(4, 5)) == List(5, 7, 3))
  }

  it should "Exercise 3.23: Generalize the function you just wrote so that it’s not specific to integers or addition. " +
    "Name your generalized function zipWith." in {

    val operation = (x1: Int, x2: Int) => (x1, x2) match {
      case (x, y) => x + y
      case (x, _) => x
      case (_, y) => y
    }

    assert(List.zipWith(List(1, 2, 3), List(4, 5, 6))(operation) == List(5, 7, 9))
    assert(List.zipWith(List(1, 2), List(4, 5, 6))(operation)  == List(5, 7, 6))
    assert(List.zipWith(List(1, 2, 3), List(4, 5))(operation)  == List(5, 7, 3))
  }

}
