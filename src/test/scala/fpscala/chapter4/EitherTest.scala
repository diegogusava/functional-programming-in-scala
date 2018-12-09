package fpscala.chapter4

import org.scalatest.FunSuite

class EitherTest extends FunSuite {

  test("Exercise 4.6: sequence") {
    assert(Either.sequence(List(Right(1), Right(2), Left(new RuntimeException))).isInstanceOf[Left[Exception]])
    assert(Either.sequence(List(Right(1), Right(2))) == Right(List(1, 2)))

    assert(Either.sequence_2(List(Right(1), Right(2), Left(new RuntimeException))).isInstanceOf[Left[Exception]])
    assert(Either.sequence_2(List(Right(1), Right(2))) == Right(List(1, 2)))

    assert(Either.sequence_3(List(Right(1), Right(2), Left(new RuntimeException))).isInstanceOf[Left[Exception]])
    assert(Either.sequence_3(List(Right(1), Right(2))) == Right(List(1, 2)))
  }

  test("Exercise 4.7: traverse") {
    assert(Either.traverse(List(Right(1), Right(2), Right(3)))(curr => if (curr.value == 2) Left(0) else curr) == Left(0))
    assert(Either.traverse(List(Right(1), Right(2), Right(3)))(curr => curr) == Right(List(1, 2, 3)))
  }

}