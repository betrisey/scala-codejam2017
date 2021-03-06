// Not working for large dataset 😔

package ninja.sam.codejam.R1B

import ninja.sam.codejam.Solver

import scala.collection.mutable.LinkedHashMap

object B extends Solver(inputFile = "B-small-attempt0.in") {
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

    val sorted = poneys.sortBy(c => poneys.count(_ == c))

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

    var ok = (newArray.head != newArray.last||newArray.length == 1)
    var i = 0
    while (ok&&i < poneys.length - 1) {
      ok = newArray(i) != newArray(i+1)
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
