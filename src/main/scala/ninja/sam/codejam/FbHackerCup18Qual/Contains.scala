package ninja.sam.codejam.FbHackerCup18Qual

import ninja.sam.codejam.Solver

object Contains extends Solver(inputFile = "ethan_searches_for_a_string_final.in") {
  override type Input = String
  override type Output = Option[String]

  override def read = {
    in.next[String]
  }

  override def apply(input: Input): Output = {

    def contains(term: String, string: String, i:Int = 0, j:Int = 0): Boolean = {
      if (i >= term.length) return true
      if (j >= string.length) return false
      if (term.charAt(i) == string.charAt(j))
        return contains(term, string, i+1, j+1)
      if (i == 0)
        return contains(term, string, i, j+1)
      return contains(term, string, 0, j)
    }

    val falseNegative = input.head + input.tail.takeWhile(_ != input.head) + input
    val result = contains(input, falseNegative)

    //println(s"contains($input, $falseNegative) = $result")

    if (!result)
      Some(falseNegative)
    else
      None
  }

  override def format(output: Output) = output.getOrElse("Impossible")
}

case class Attraction(var visit: Int, popularity: Int, name: String)