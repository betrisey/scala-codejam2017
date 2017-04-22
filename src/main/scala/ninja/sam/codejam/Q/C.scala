package ninja.sam.codejam.Q

import ninja.sam.codejam.Solver

import scala.collection.mutable

object C extends Solver(inputFile = "C-small-2-attempt0.in") {
  override type Input = (Int, Int)
  override type Output = (Long, Long)

  override def read = {
    in.next[Int] -> in.next[Int]
  }

  override def apply(input: Input) = {
    var stalls = Array.fill[Boolean](input._1 + 2)(false)
    stalls(0) = true
    stalls(input._1 + 1) = true
    val people = input._2

    var min = Long.MinValue
    var max = Long.MaxValue

    var emptyStalls = mutable.Set.empty[Int]
    emptyStalls ++= stalls.zipWithIndex.filter{case(b, i) => !b}.map(_._2)

    def findStall(): (Int) = {
      min = Long.MinValue
      max = Long.MaxValue
      var chosen = -1

      for(i <- emptyStalls) {
        val right = stalls.drop(i + 1).takeWhile(!_).length.toLong
        val left = stalls.reverse.drop(stalls.length - i).takeWhile(!_).length.toLong
        val newMin = Math.min(left, right)
        val newMax = Math.max(left, right)

        if (newMin > min) {
          min = newMin
          max = newMax
          chosen = i
        } else if (newMin == min && newMax > max) {
          max = newMax
          chosen = i
        }
      }

      emptyStalls.remove(chosen)
      chosen
    }

    for (i <- 0 until(people)) {
      stalls(findStall()) = true
    }

    (max, min)
  }

  override def format(output: Output) = output._1 + " " + output._2
}
