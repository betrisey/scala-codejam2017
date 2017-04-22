package ninja.sam.codejam.Q

import ninja.sam.codejam.Solver

import scala.collection.mutable

object ABruteForce extends Solver(inputFile = "A-small-attempt0.in") {
  override type Input = (Array[Boolean], Int)
  override type Output = Option[Int]

  override def read = {
    (in.next().toCharArray.map(_ == '+'), in.next().toInt)
  }

  override def apply(input: Input): Output = {
    val initPancakes = input._1
    val spatule = input._2

    var answer:Option[Int] = None

    def flip(pancakes: Array[Boolean], depth: Int, alreadyTested: mutable.Set[mutable.WrappedArray[Boolean]]): Unit = {
      if (!answer.isEmpty && answer.get <= depth) {
        return
      }

      if (pancakes.reduce(_ && _)) {
        // We have a solution
        if (answer.isEmpty || answer.get > depth) {
          // A better one
          answer = Some(depth)
        }
        return
      }

      for (i <- 0 to pancakes.length - spatule) {
        // Flip the pancakes 🍳🥞
        val newPancakes = pancakes.zipWithIndex.map{case(e, pos) => e^(pos >= i && pos < i+spatule)}
        if (!alreadyTested.contains(newPancakes)) {
          flip(newPancakes, depth+1, alreadyTested + newPancakes)
        }
      }
    }

    flip(initPancakes, 0, mutable.Set.empty[mutable.WrappedArray[Boolean]] + initPancakes)

    answer
  }

  override def format(output: Output) = output.map(_.toString) getOrElse "IMPOSSIBLE"
}
