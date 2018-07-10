package ninja.sam.codejam.FbHackerCup18Qual

import ninja.sam.codejam.Solver

object Interception extends Solver(inputFile = "interception_example_final.in") {
  override type Input = Array[Int]
  override type Output = Array[Double]

  override def read = {
    val n = in.next[Int]
    (new Array[Int](n+1)).map(_ => in.next[Int])
  }

  override def apply(input: Input): Output = {
    if ((input.length-1) % 2 == 0)
      new Array[Double](0) // No solutions
    else
      new Array[Double](1) // Unique solution x = 0
  }

  override def format(output: Output) = {
    if (output.isEmpty) "0"
    else output.length + "\n" + output.mkString("\n")
  }
}
