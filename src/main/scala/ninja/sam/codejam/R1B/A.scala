package ninja.sam.codejam.R1B

import ninja.sam.codejam.Solver

object A extends Solver(inputFile = "A-large.in") {
  override type Input = (Int, Array[(Int, Int)])
  override type Output = Double

  override def read = {
    val d = in.next[Int]
    val n = in.next[Int]
    val horses = (new Array[(Int, Int)](n)).map(x => (in.next[Int], in.next[Int]))
    (d, horses)
  }

  override def apply(input: Input): Output = {
    val d = input._1
    val horses = input._2

    // When the last 🐎 will arrive
    val time = horses.map{case (pos, speed) => (d-pos).toDouble / speed.toDouble}.max
    d / time
  }

  override def format(output: Output) = "%.6f".format(output)
}
