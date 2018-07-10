package ninja.sam.codejam.FbHackerCup18Qual

import ninja.sam.codejam.Solver

object Tourist extends Solver(inputFile = "tourist_example_input_final.in") {
  override type Input = (Int, BigInt, Array[Attraction])
  override type Output = Array[String]

  override def read = {
    val n = in.next[Int]
    val k = in.next[Int]
    val v = in.next[BigInt]
    val attractions = (new Array[String](n)).zipWithIndex.map(x => Attraction(0, x._2 + 1, in.next[String]))
    (k, v, attractions)
  }

  override def apply(input: Input): Output = {
    val k = input._1
    val vInit = input._2
    var attractions = input._3
    val n = attractions.length

    if (n == k)
      return attractions.map(_.name)

    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
    def lcm(a: Int, b: Int) = (a*b)/gcd(a, b)

    val period = n*lcm(k, n-k)/(n-k)/k
    var v = (vInit % period).intValue()
    if (v == 0) v = period

    for (_ <- 1 to v - 1) {
      //println("Before")
      //println(attractions
      //  .sortBy(x => (x.visit, x.popularity)).map(_.toString).mkString("\n"))
      attractions = attractions
        .sortBy(x => (x.visit, x.popularity))
        .zipWithIndex
        .map(x => {
          if (x._2 < k)
            x._1.visit += 1
          x._1
        })

      //println("After")
      //println (attractions.map(_.toString).mkString("\n"))
      //println()
    }

    attractions
      .sortBy(x => (x.visit, x.popularity))
      .take(k)
      .sortBy(_.popularity)
      .map(_.name)
  }

  override def format(output: Output) = output.mkString(" ")
}

case class Attraction(var visit: Int, popularity: Int, name: String)