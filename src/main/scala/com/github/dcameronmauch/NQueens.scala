package com.github.dcameronmauch

import scala.annotation.tailrec

object NQueens {
  def nQueens(n: Int): Set[Set[Int]] = {
    case class State(queens: Set[Int], avail: Set[Int])

    def filter(p: Int, avail: Set[Int]): Set[Int] = avail.filterNot(a => {
      val sameRow: Boolean = (p / n) == (a / n)
      val sameCol: Boolean = (p % n) == (a % n)
      val diagonal: Boolean = ((p / n) - (a / n)).abs == ((p % n) - (a % n)).abs
      sameRow || sameCol || diagonal
    })

    @tailrec
    def recurse(todo: List[State], acc: Set[Set[Int]]): Set[Set[Int]] = todo match {
      case Nil =>
        acc
      case State(queens, _) :: tail if (queens.size == n) =>
        recurse(tail, acc + queens)
      case State(queens, avail) :: tail if (avail.size < n - queens.size) =>
        recurse(tail, acc)
      case State(queens, avail) :: tail =>
        val next: List[State] = avail.toList.map(a =>
          State(queens + a, filter(a, avail - a))
        )
        recurse(next ++ tail, acc)
    }

    val range: Set[Int] = (0 until n * n).toSet
    recurse(List(State(Set.empty, range)), Set.empty)
  }
}
