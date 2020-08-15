package ninja.sam.codejam.FbHackerCup19Qual

import ninja.sam.codejam.Solver

object Leapfrog1 extends Solver(inputFile = "leapfrog_ch_.txt") {
  override type Input = Array[Boolean]
  override type Output = Boolean

  override def read = {
    in.next[String].tail.toCharArray.map(_ == 'B')
  }

  override def apply(input: Input): Output = {
    val bCount = input.count(_ == true)

    return bCount < input.length && bCount >= (input.length / 2.0).ceil.toInt
  }

  override def format(output: Output) = if (output) "Y" else "N"
}