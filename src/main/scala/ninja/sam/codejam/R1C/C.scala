package ninja.sam.codejam.R1C

import ninja.sam.codejam.Solver

object C extends Solver(inputFile = "C-small-1-attempt0.in") {
  override type Input = (Double, Array[Double])
  override type Output = Double

  override def read = {
    val n = in.next[Int]
    val k = in.next[Int]
    val u = in.next[Double]
    val cores = (new Array[Double](n)).map(x => in.next[Double])
    (u, cores)
  }

  override def apply(input: Input): Output = {
    var u = input._1
    var cores = input._2

    // 1.4
    // 0.5 0.6 0.7 0.8
    while (u > 0) {
      cores = cores.sorted
      var trainingCount = cores.takeWhile(x => x == cores.head).length
      var amount = u
      if (cores.length > trainingCount) {
        amount = Math.min((cores(trainingCount) - cores(0)) * trainingCount, u)
      }

      u -= amount
      val amountByCore = amount / trainingCount
      cores = cores.zipWithIndex.map(x => if (x._2 < trainingCount) x._1 + amountByCore else x._1)
    }

    cores.reduce(_*_)
  }

  override def format(output: Output) = "%.6f".format(output)
}