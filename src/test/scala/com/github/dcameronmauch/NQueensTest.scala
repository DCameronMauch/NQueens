package com.github.dcameronmauch

import org.scalatest.funsuite.AnyFunSuite

class NQueensTest extends AnyFunSuite {
  test("n = 3") {
    val result: Set[Set[Int]] = NQueens.nQueens(3)
    val expected: Set[Set[Int]] = Set.empty
    assertResult(expected)(result)
  }

  test("n = 4") {
    val result: Set[Set[Int]] = NQueens.nQueens(4)
    val expected: Set[Set[Int]] = Set(
      Set(1, 7, 8, 14),
      Set(2, 4, 11, 13)
    )
    assertResult(expected)(result)
  }

  test("n = 5") {
    val result: Set[Set[Int]] = NQueens.nQueens(5)
    val expected: Set[Set[Int]] = Set(
      Set(3, 6, 14, 17, 20),
      Set(1, 8, 10, 17, 24),
      Set(0, 7, 14, 16, 23),
      Set(0, 8, 11, 19, 22),
      Set(4, 7, 10, 18, 21),
      Set(2, 5, 13, 16, 24),
      Set(2, 9, 11, 18, 20),
      Set(4, 6, 13, 15, 22),
      Set(1, 9, 12, 15, 23),
      Set(3, 5, 12, 19, 21)
    )
    assertResult(expected)(result)
  }
}
