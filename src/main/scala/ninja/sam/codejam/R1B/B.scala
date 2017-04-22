// Not working for large dataset 😔

package ninja.sam.codejam.R1B

import ninja.sam.codejam.Solver
import scala.collection.mutable.LinkedHashMap

object B extends Solver(inputFile = "B-large.in") {
  override type Input = Map[Char, Int]
  override type Output = Option[String]

  override def read = {
    in.next[Int] // N
    LinkedHashMap(('R', 0), ('O', 0), ('Y', 0), ('G', 0), ('B', 0), ('V', 0))
      .mapValues(x => in.next[Int])
      .filter{case(c, n) => n != 0}
      .toMap
  }

  override def apply(input: Input): Output = {
    var poneys = input.flatMap{case (c, n) => c.toString * n}.toList

    def countSameHair(c: Char) : Int = {
      if (c == 'R'||c == 'Y'||c == 'B') {
        return poneys.count(_ == c)
      } else if (c == 'O') {
        return poneys.count(x => x == 'R'||x == 'Y'||x == 'O')
      } else if (c == 'G') {
        return poneys.count(x => x == 'Y'||x == 'B'||x == 'G')
      } else if (c == 'V') {
        return poneys.count(x => x == 'R'||x == 'B'||x == 'V')
      }
      return 0
    }

    def sameHair (c1: Char, c2: Char): Boolean = {
      if (c1 == c2) return true

      if (c1 == 'O') {
        return c2 == 'R'||c2 == 'Y'
      }
      if (c1 == 'G') {
        return c2 == 'Y'||c2 == 'B'
      }
      if (c1 == 'V') {
        return c2 == 'R'||c2 == 'B'
      }

      if (c2 == 'O') {
        return c1 == 'R'||c1 == 'Y'
      }
      if (c2 == 'G') {
        return c1 == 'Y'||c1 == 'B'
      }
      if (c2 == 'V') {
        return c1 == 'R'||c1 == 'B'
      }

      return false
    }

    val sorted = poneys.sortBy(countSameHair)

    val newArray = new Array[Char](sorted.length)

    for (i <- 0 until poneys.length) {
      if (i * 2 < poneys.length) {
        newArray(i * 2) = sorted(i)
      } else {
        if (poneys.length % 2 == 0) {
          newArray((i * 2) % poneys.length + 1) = sorted(i)
        } else {
          newArray((i * 2) % poneys.length) = sorted(i)
        }
      }
    }

    var ok = (!sameHair(newArray.head, newArray.last)||newArray.length == 1)
    var i = 0
    while (ok&&i < poneys.length - 1) {
      ok = !sameHair(newArray(i), newArray(i+1))
      i += 1
    }

    if (ok) {
      Some(newArray.mkString)
    } else {
      None
    }
  }

  override def format(output: Output) = if (output.isEmpty) "IMPOSSIBLE" else output.get
}
