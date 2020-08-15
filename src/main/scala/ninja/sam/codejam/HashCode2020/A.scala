package ninja.sam.codejam.HashCode2020

import ninja.sam.codejam.Solver

object A extends Solver(inputFile = "A-large.in") {
  type Pancake = (Int, Int)
  override type Input = (Int, Array[Pancake]) // order, pancakes (r, h)
  override type Output = Double

  override def read = {
    val n = in.next[Int]
    val order = in.next[Int]
    val pancakes = (new Array[Pancake](n)).map(x => (in.next[Int], in.next[Int]))
    (order, pancakes)
  }

  override def apply(input: Input): Output = {
    val order = input._1
    var pancakes = input._2

    def sideArea(pancake: Pancake): Double = {
      val r = pancake._1.toDouble
      val h = pancake._2.toDouble
      2*Math.PI*r*h
    }

    def topArea(pancake: Pancake): Double = {
      val r = pancake._1.toDouble
      Math.PI*r*r
    }

    def totalArea(pancakes: List[Pancake]): Double = {
      topArea(pancakes.maxBy(x => x._1)) + pancakes.map(x => sideArea(x)).reduce(_+_)
    }

    def sort(pancake: Pancake): Double = {
      sideArea(pancake)
    }

    pancakes.zipWithIndex.filter(e => {
      val r = e._1._1
      pancakes.count(x => x._1 <= r) >= order
    }).map(e => {
      val bottom = e._1
      val remainingPancakes = pancakes.zipWithIndex.collect { case(a, i) if i != e._2 => a}
      totalArea(bottom :: remainingPancakes.sortBy(sort).reverse.take(order - 1).toList)
    }).max
  }

  override def format(output: Output) = "%.9f".format(output)
}
