package ninja.sam.codejam.Q

import ninja.sam.codejam.Solver

object B extends Solver(inputFile = "B-large.in") {
  override type Input = Array[Int]
  override type Output = Long

  override def read = {
    in.next[String].toCharArray.map(_.toString.toInt)
  }

  override def apply(input: Input): Output = {
    for (i <- input.length-1 to 1 by -1 ) {
      if (input(i-1) > input(i)) {
        var j = i
        while (j < input.length && input(j) != 9) {
          input(j) = 9
          j += 1
        }
        input(i-1) = input(i-1)-1
      }
    }

    input.dropWhile(_ == 0).mkString.toLong
  }
}
