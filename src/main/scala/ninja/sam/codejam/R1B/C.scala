// Not yet implemented

package ninja.sam.codejam.R1B

import ninja.sam.codejam.Solver

object C extends Solver(inputFile = "C.in") {
  override type Input = (Array[(Int, Int)], Array[Array[Int]], Array[(Int, Int)])
  override type Output = Array[Double]

  override def read = {
    val n = in.next[Int] // number of cities with horses
    val q = in.next[Int] // number of pairs of stops we are interested in

    // Ei, the maximum total distance, in kilometers, the horse in the i-th city can go
    // Si, the constant speed, in kilometers per hour, at which the horse travels.
    val pairs = (1 until n + 1).map {
      _ => in.next[Int] -> in.next[Int]
    }.toArray


    // Dij, is -1 if there is no direct route from the i-th to the j-th city,
    // and the length of that route in kilometers otherwise.
    var routes = Array.ofDim[Int](n, n)
    for {
      i <- 0 until n
      j <- 0 until n
    } routes(i)(j) = in.next[Int]

    // containing two integers Uk and Vk, the starting and destination point
    // respectively, of the k-th pair of cities we want to investigate.
    val startEndPairs = (0 until q).map {
      _ => in.next[Int] -> in.next[Int]
    }.toArray

    (pairs, routes, startEndPairs)
  }

  override def apply(input: Input): Output = {
    input._3.map{case (start, end) => 0.toDouble}
  }

  override def format(output: Output) = output.map(x => "%.6f".format(x)).mkString(" ")
}
