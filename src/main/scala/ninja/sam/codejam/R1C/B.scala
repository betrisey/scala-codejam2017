package ninja.sam.codejam.R1C

import ninja.sam.codejam.Solver

object B extends Solver(inputFile = "B.in") {
  type Activity = (Int, Int)
  override type Input = (Array[Activity], Array[Activity])
  override type Output = Int

  override def read = {
    val n1 = in.next[Int]
    val n2 = in.next[Int]
    val a1 = (new Array[Activity](n1)).map(x => (in.next[Int], in.next[Int]))
    val a2 = (new Array[Activity](n2)).map(x => (in.next[Int], in.next[Int]))
    (a1, a2)
  }

  override def apply(input: Input): Output = {
    val a1 = input._1
    val a2 = input._2

    val parents = Array(a1, a2)
    val timeParent = Array(0, 0)

    def isFree(activities: Array[Activity], time: Int): Boolean = {
      activities.filter(x => time > x._1 && time < x._2).isEmpty
    }

    class Switch (last: Int, p: Int) {
      val firstTime: Int = {
        var time = last
        while (time >= 0 && isFree(parents(p), time)) {
          time -= 1
        }
        time + 1
      }
      val lastTime = last
      val parent = p
    }

    var switches = List[Switch]()
    var count = 0
    var turn = 0
    for (time <- 0 until 1440) {
      if (!isFree(parents(turn), time)) {
        turn ^= 1
        switches = new Switch(time, turn) :: switches
      }
      timeParent(turn) += 1
    }

    var diff = timeParent(0) - timeParent(1)
    while (diff != 0) {
      if (diff > 0) {
        // p 0 has too much
        if (Math.abs(switches.filter(_.parent == 1).map(x => x.lastTime - x.firstTime).sum) >= Math.abs(diff)) {
          return count
        }

        // TODO: Add switch

      } else {
        // p 1 has too little
        if (Math.abs(switches.filter(_.parent == 0).map(x => x.lastTime - x.firstTime).sum) >= Math.abs(diff)) {
          return count
        }

        // TODO: Add switch
      }
      // TODO: Compute new diff
    }

    count
  }


  override def format(output: Output) = output.toString
}