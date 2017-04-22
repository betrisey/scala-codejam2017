package ninja.sam.codejam.Q

import ninja.sam.codejam.Solver

object Cv2 extends Solver(inputFile = "C-large.in") {
  override type Input = (Long, Long) // (Stalls, People)
  override type Output = (Long, Long)

  override def read = {
    in.next[Long] -> in.next[Long]
  }

  override def apply(input: Input): Output = {
    // All stalls will be taken
    if (input._1 == input._2) return (0, 0)

    val n = input._1 - 1 // Stalls - 1
    val a = n / 2
    val b = n - a

    val k = input._2 - 1
    val ka = k / 2
    val kb = k - ka

    if (input._2 == 1) return (b, a)

    if (input._2 == 2) return apply((b, 1))

    if (n % 2 == 0) {
      return apply((b, kb))
    } else {
      if (k % 2 == 0) {
        return apply((a, ka))
      } else {
        return apply((b, kb))
      }
    }
  }

  override def format(output: Output) = output._1 + " " + output._2
}
