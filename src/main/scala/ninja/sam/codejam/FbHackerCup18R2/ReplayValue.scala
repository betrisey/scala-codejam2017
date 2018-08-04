package ninja.sam.codejam.FbHackerCup18R2

import better.files._

object ReplayValue extends App {
  val inputFile = "replay_value_sample.in"
  type Input = ReplayValueInput
  type Output = Int

  def read: Input = {
    val n = in.next[Int]
    val s = in.next[Int]
    val e = in.next[Int]
    val lasers = (0 until n).map(_ => (in.next[Int], in.next[Int])).toList
    return ReplayValueInput(s, e, lasers)
  }

  def apply(input: Input): Output = {

    return 0 % 1000000007
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

case class ReplayValueInput(start: Int, End: Int, lasers: List[(Int, Int)])
