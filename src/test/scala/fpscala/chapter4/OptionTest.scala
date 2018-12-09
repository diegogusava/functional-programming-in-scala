package fpscala.chapter4

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  test("map") {
    assert(Some(1).map(v => v + 1) == Some(2))
    val option: Option[Int] = None
    assert(option.map(v => v + 1) == None)
  }

  test("flatMap") {
    assert(Some(1).flatMap(v => Some(v + 1)) == Some(2))
    assert(None.flatMap(v => Some(1)) == None)
  }


  test("getOrElse") {
    assert(None.getOrElse(0) == 0)
    assert(Some(1).getOrElse(0) == 1)
  }


  test("orElse") {
    assert(None.orElse(Some(1)) == Some(1))
    assert(Some(1).orElse(None) == Some(1))
  }

  test("filter") {
    val option: Option[Int] = None
    assert(option.filter(x => x > 0) == None)
    assert(Some(1).filter(x => x > 0) == Some(1))
  }

  test("Exercise 4.2: Implement the variance function in terms of flatMap.") {

  }

  test("Exercise 4.4: Write a function sequence that combines a list of Options into one Option containing " +
    "a list of all the Some values in the original list. If the original list contains None even once, the result of " +
    "the function should be None; otherwise the result should be Some with a list of all the values. ") {

    val result1: Option[List[Int]] = Option.sequence_2(List(Some(1), None, Some(2)))
    assert(result1 == None)

    val result2: Option[List[Int]] = Option.sequence_2(List(Some(1), Some(2)))
    assert(result2 == Some(List(1, 2)))
  }

  test("traverse") {
    def process(x: String): Option[Int] = Option.Try(x.toInt)

    Option.traverse2[String, Int](List("1", "a", "2"))(process)
  }

  test("sequence traverse") {
    val result1: Option[List[Int]] = Option.sequenceTraverse(List(Some(1), None, Some(2)))
    assert(result1 == None)

    val result2: Option[List[Int]] = Option.sequenceTraverse(List(Some(1), Some(2)))
    assert(result2 == Some(List(1, 2)))
  }
}
