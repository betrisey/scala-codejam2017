package ninja.sam.codejam.Q

import ninja.sam.codejam.Solver

object A extends Solver(inputFile = "A-large.in") {
  override type Input = (Array[Boolean], Int)
  override type Output = Option[Int]

  override def read = {
    (in.next().toCharArray.map(_ == '+'), in.next().toInt)
  }

  override def apply(input: Input): Output = {
    val initPancakes = input._1
    val spatula = input._2

    var count = 0
    var pancakes = initPancakes
    // 🥞 left to flip ?
    while (pancakes.length >= spatula) {
      // Remove the 😀🥞
      pancakes = pancakes.dropWhile(x => x)

      // 🥞 left to flip ?
      if (pancakes.length >= spatula) {
        // Flip it 🍳
        pancakes = pancakes.zipWithIndex.map{case(e, pos) => e ^ (pos < spatula)}
        count += 1
      }
    }

    if (pancakes.isEmpty) Some(count) else None
  }

  override def format(output: Output) = output.map(_.toString) getOrElse "IMPOSSIBLE"
}
