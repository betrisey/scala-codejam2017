package ninja.sam.codejam.FbHackerCup19R1

import better.files._

object GraphsAsAService extends App {
  val inputFile = "graphs_as_a_service_sample_input.txt"
  type Input = (Int, Array[Constraint])
  type Output = Array[Constraint]

  def read = {
    var n = in.next[Int]
    val m = in.next[Int]

    val graph = (0 until m).map(_ => Constraint(in.next[Int], in.next[Int], in.next[Int])).toArray
    (n, graph)
  }

  def apply(input: Input): Output = {
    Array.empty
  }

  case class Constraint(x: Int, y: Int, weight: Int) {
    override def toString: String = {
      x + " " + y + " " + weight
    }
  }

  def format(output: Output) = if (output.isEmpty) "IMPOSSIBLE" else output.map(_.toString).mkString("\n")

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
