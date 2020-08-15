package ninja.sam.codejam.FbHackerCup19R1

import ninja.sam.codejam.Solver

object PlatformParkour extends Solver(inputFile = "platform_parkour_sample.in") {
  override type Input = ParkoutInput
  override type Output = Double

  override def read = {
    val n = in.next[Int]
    val m = in.next[Int]

    val h1 = in.next[Int]
    val h2 = in.next[Int]

    val w = in.next[Long]
    val x = in.next[Long]
    val y = in.next[Long]
    val z = in.next[Long]

    val input = new Array[Array[Int]](m).map(_ => new Array[Int](4).map(_ => in.next[Int])).transpose
    val a = input(0).map(_ - 1) // zero-index
    val b = input(1).map(_ - 1)
    val u = input(2)
    val d = input(3)

    val h = new Array[Int](n)
    h(0) = h1
    h(1) = h2
    for (i <- 2 until n) {
      h(2) = ((w * h(i-2) + x * h(i-1) + y) % z).toInt
    }

    ParkoutInput(a, b, u, d, h)
  }

  override def apply(input: Input): Output = {
    0
  }

  override def format(output: Output) = output.toString
}

case class ParkoutInput(a: Array[Int], b: Array[Int], u: Array[Int], d: Array[Int], h: Array[Int])