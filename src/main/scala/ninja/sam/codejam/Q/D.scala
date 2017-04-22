// Not yet implemented

package ninja.sam.codejam.Q

import ninja.sam.codejam.Solver

object D extends Solver(inputFile = "D.in") {
  override type Input = String
  override type Output = Input

  override def read = {
    in.next()
  }

  override def apply(input: Input) = {
    input
  }
}
