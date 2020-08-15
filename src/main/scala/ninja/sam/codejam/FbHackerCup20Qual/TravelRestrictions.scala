package ninja.sam.codejam.FbHackerCup20Qual

import ninja.sam.codejam.Solver

object TravelRestrictions extends Solver(inputFile = "travel_restrictions_input.txt") {
  override type Input = (Int, Array[Boolean], Array[Boolean])
  override type Output = Array[Array[Boolean]]

  override def read = {
    (
      in.next[Int], // N
      in.next[String].toCharArray.map(_ == 'Y'), // I
      in.next[String].toCharArray.map(_ == 'Y')  // O
    )
  }

  override def apply(input: Input): Output = {
    val n = input._1
    val is = input._2
    val os = input._3
    val output = Array.ofDim[Boolean](n, n)

    for (i <- 0 until n) {
      output(i)(i) = true
    }

    for { l <- 1 until n
          i <- 0 until n} {
      val j1 = i + l
      if (j1 < n) {
        if (l == 1) {
          output(i)(j1) = os(i) && is(j1)
        } else {
          output(i)(j1) = output(i)(i+1) && output(i+1)(j1)
        }
      }

      val j2 = i - l
      if (j2 >= 0) {
        if (l == 1) {
          output(i)(j2) = os(i) && is(j2)
        } else {
          output(i)(j2) = output(i)(i-1) && output(i-1)(j2)
        }
      }
    }

    output
  }

  override def format(output: Output) = output
    .map(line => "\n" + line.map {
      case true => 'Y'
      case _ => 'N'
    }.mkString)
    .mkString
}