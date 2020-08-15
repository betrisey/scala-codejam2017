package ninja.sam.codejam.FbHackerCup19Qual

import ninja.sam.codejam.Solver

object Leapfrog2 extends Solver(inputFile = "leapfrog_ch_.txt") {
  override type Input = Array[Boolean]
  override type Output = Boolean

  override def read = {
    in.next[String].tail.toCharArray.map(_ == 'B')
  }

  override def apply(input: Input): Output = {
    val bCount = input.count(_ == true)

    return bCount < input.length && bCount >= 2 || (input.length == 2 && bCount == 1)
  }

  override def format(output: Output) = if (output) "Y" else "N"
}