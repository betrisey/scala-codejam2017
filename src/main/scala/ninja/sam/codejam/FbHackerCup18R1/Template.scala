package ninja.sam.codejam.FbHackerCup18R1

import better.files._

object Template extends App {
  val inputFile = "template.in"
  type Input = Int
  type Output = Int

  def read: Input = {
    val n = in.next[Int]
    return n
  }

  def apply(input: Input): Output = {
    return input
  }

  def format(output: Output): String = output.toString

  /* Template */
  val in = Scanner(getClass.getResourceAsStream(inputFile))(Scanner.Config.default)
  val out = File(inputFile.replace(".in", ".out")).newOutputStream.printer()

  println(s"Solving: $inputFile")
  for (i <- 1 to in.next[Int]) {
    val output = s"Case #$i: ${format(apply(read))}"
    println(output)
    out.println(output)
  }
  in.close()
  out.close()
}
