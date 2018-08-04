package ninja.sam.codejam.FbHackerCup18R2

import better.files._

import scala.collection.mutable.ListBuffer

object EthanShortestPath extends App {
  val inputFile = "ethan_finds_the_shortest_path_final.in"
  type Input = (Int, Int)
  type Output = (Int, List[Edge])

  def read: Input = {
    (in.next[Int], in.next[Int]) // (n, k)
  }

  def apply(input: Input): Output = {
    val (n, k) = input

    def ethansAlgorithm(edges: List[Edge], i: Int, d: Int): Int = {
      // 2. If i is equal to N, output d and stop
      if (i == n - 1)
        return d

      // 3. Find the edge incident to node i that has the smallest weight (if no edges are incident to i or if there are multiple such edges tied with the smallest weight, then crash instead)
      val edgesFound = edges.filter(e => e.u == i || e.v == i).sortBy(_.weight)
      if (edgesFound.isEmpty || (edgesFound.length > 1 && edgesFound(0).weight == edgesFound(1).weight)) {
        return -1 // Crash
      }
      val edge = edgesFound(0)
      // 4. Increase d by the weight of this edge, and set i to be equal to the other node incident to this edge
      // 5. Return to Step 2
      return ethansAlgorithm(edges, if (edge.u == i) edge.v else edge.u, d + edge.weight)
    }

    // Generate graph
    val edges = ListBuffer[Edge]()
    edges += Edge(0, n-1, k) // start to end edge, Ethan won't take this one
    if (n > 2) {
      val maxSteps = Math.min(n-1, k-1)
      if ((0 until maxSteps).map(k-1-_).sum > k) { // Ethan will find a worse path
        for (i <- 0 until maxSteps) {
          edges += Edge(i, if (i == maxSteps-1) n-1 else i+1, k-1-i)
        }
      }
    }
    val edgeList = edges.toList

    return (ethansAlgorithm(edgeList, 0, 0) - k, edgeList)
  }

  case class Edge(u: Int, v: Int, weight: Int) {
    override def toString: String = (u+1) + " " + (v+1) + " " + weight // 1-indexed
  }

  def format(output: Output): String = {
    output._1.toString + "\n" + output._2.length + "\n" + output._2.mkString("\n")
  }

  /* Template */
  val in = Scanner(getClass.getResourceAsStream(inputFile))(Scanner.Config.default)
  val out = File(inputFile.replace(".in", ".out")).newOutputStream.printer()

  println(s"Solving: $inputFile")
  for (i <- 1 to in.next[Int]) {
    val output = s"Case #$i: ${format(apply(read))}"
    println(output)
    out.println(output)
  }
  in.close()
  out.close()
}
