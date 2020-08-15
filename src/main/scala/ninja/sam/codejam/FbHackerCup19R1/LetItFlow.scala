package ninja.sam.codejam.FbHackerCup19R1

import ninja.sam.codejam.Solver

object LetItFlow extends Solver(inputFile = "let_it_flow_final.in") {
  override type Input = Array[Array[Boolean]]
  override type Output = Int

  override def read = {
    in.next[Int]
    new Input(3)
      .map(_ => in.next[String].toCharArray.map(_ == '#'))
      .transpose
  }

  override def apply(input: Input): Output = {
    if (input.last.last) return 0 // exit blocked

    /**
      * @param grid
      * @param pos vertical position (0,1,2)
      * @return solution count
      */
    def nextStep(grid: Input, pos: Int): Int = {
      val col = grid.head
      if (grid.length == 1) {
        if (pos == 1 && !col(1) && !col(2)) return 1
        return 0
      }

      if (col(pos) || col(1)) // pipe blocked by a wall
        return 0

      if (pos == 1) {
        // Only one possibility
        if (!col(0) && col(2))
          return nextStep(grid.tail, 0)
        if (!col(2) && col(0))
          return nextStep(grid.tail, 2)

        // Two possibilities that will merge at the next step (pos=1)
        if (grid.length > 2) {
          val nextCol = grid(1)
          if (nextCol(1)) return 0
          if (!nextCol(0) && !nextCol(2))
            return (2 * nextStep(grid.tail.tail, 1)) % 1000000007
          if (!nextCol(0))
            return nextStep(grid.tail.tail, 1)
          if (!nextCol(2))
            return nextStep(grid.tail.tail, 1)
          return 0
        } else { // length == 2 => last pipe will be in the middle
          return 0
        }
      }

      return nextStep(grid.tail, 1)
    }

    nextStep(input, 0)
  }

  override def format(output: Output) = output.toString
}