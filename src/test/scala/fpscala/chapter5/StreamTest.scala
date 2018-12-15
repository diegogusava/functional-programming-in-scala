package fpscala.chapter5

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("Exercise 5.1: Write a function to convert a Stream to a List") {
    val list = Stream(1, 2, 3).toList
    assert(list == List(1, 2, 3))
  }

  test("Exercise 5.2: Write the function take(n) for returning the first n elements of a Stream, " +
    "and drop(n) for skipping the first n elements of a Stream. ") {

    assert(Stream(1, 2, 3).take(0) == List())
    assert(Stream(1, 2, 3).take(1) == List(1))
    assert(Stream(1, 2, 3).take(3) == List(1, 2, 3))

    assert(Stream(1, 2, 3).drop(0) == List(1, 2, 3))
    assert(Stream(1, 2, 3).drop(1) == List(2,3))
    assert(Stream(1, 2, 3).drop(3) == List())
    assert(Stream(1, 2, 3).drop(4) == List())
  }

  test("Exercise 5.3: Write the function takeWhile for returning all starting elements of a Stream that " +
    "match the given predicate. ") {
    assert(Stream(1, 2, 3).takeWhile(x => x > 0).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).takeWhile(x => x < 2).toList == List(1))
    assert(Stream(1, 2, 3).takeWhile(_ > 4).toList == List())
  }

  test("Exercise 5.4: Implement forAll, which checks that all elements in the Stream match a given predicate"){
    assert(Stream(1, 2, 3).forAll(x => x > 0) == true)
    assert(Stream(1, 2, 3).forAll(x => x > 0 && x < 3) == false)
    assert(Stream(1, 2, 3).forAll(x => x > 3) == false)
  }

  test("Exercise 5.5: Use foldRight to implement takeWhile."){
    assert(Stream(1, 2, 3).takeWhile2(x => x > 0).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).takeWhile2(x => x < 2).toList == List(1))
    assert(Stream(1, 2, 3).takeWhile2(_ > 4).toList == List())
  }

}
