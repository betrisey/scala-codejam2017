package ninja.sam.codejam.FbHackerCup19R1

import ninja.sam.codejam.Solver

object LetItFlowBad extends Solver(inputFile = "let_it_flow_sample.in") {
  override type Input = Array[Array[Boolean]]
  override type Output = Int

  override def read = {
    in.next[Int]
    new Input(3).map(_ => in.next[String].toCharArray.map(_ == '#'))
  }

  override def apply(input: Input): Output = {
    if (input.last.last) return 0 // exit blocked

    def placeNext(grid: Input, x: Int, y: Int, direction: Directions.Value): Int = {
      if (grid.last.last) return 1
      val cells = nextDirections(direction).flatMap(dir => nextCell(grid, x, y, dir).map(cell => placeNext(place(grid, x, y), cell._1, cell._2, dir)))
      cells.sum % 1000000007
    }

    def place(grid: Input, x: Int, y: Int): Input = {
      //println(grid.map(_.map(if(_)'#'else'.').mkString).mkString("\n"))
      //println()
      grid.updated(y, grid(y).updated(x, true))
    }

    def nextCell(grid: Input, x: Int, y: Int, direction: Directions.Value): Option[(Int, Int)] = {
      var newX = x
      var newY = y
      if (direction == Directions.Left)
        newX -= 1
      else if (direction == Directions.Right)
        newX += 1
      else if (direction == Directions.Up)
        newY -= 1
      else if (direction == Directions.Down)
        newY += 1

      if (newX < 0 || newY < 0 || newX >= grid(0).length || newY >= grid.length || grid(newY)(newX))
        return None
      else Some((newX, newY))
    }

    def nextDirections(direction: Directions.Value): List[Directions.Value] = {
      if (direction == Directions.Up || direction == Directions.Down)
        return List(Directions.Left, Directions.Right)
      else
        return List(Directions.Up, Directions.Down)
    }

    placeNext(input, 0, 0, Directions.Right)
  }

  override def format(output: Output) = output.toString

  object Directions extends Enumeration {
    val Left, Right, Up, Down = Value
  }
}