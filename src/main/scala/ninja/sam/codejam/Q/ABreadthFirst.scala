package ninja.sam.codejam.Q

import ninja.sam.codejam.Solver

import scala.collection.mutable
import scala.collection.immutable.Queue

object ABreadthFirst extends Solver(inputFile = "A-large.in") {
  override type Input = (Array[Boolean], Int)
  override type Output = Option[Int]

  override def read = {
    (in.next().toCharArray.map(_ == '+'), in.next().toInt)
  }

  override def apply(input: Input): Output = {
    val initPancakes = input._1
    val spatula = input._2

    var seen = mutable.Set.empty[mutable.WrappedArray[Boolean]] + initPancakes

    def newPancakes(pancakes: Array[Boolean], i: Int): Option[Array[Boolean]] = {
      if (seen.size >= 10000) {return None}
      val nextPancake = pancakes.zipWithIndex.map{case(e, pos) => e ^ (pos >= i && pos < i + spatula)}
      if (seen.add(nextPancake)) {
        return Some(nextPancake)
      } else {
        return None
      }
    }

    def getChildren (pancakes: (Array[Boolean], Int)) : Stream[(Array[Boolean], Int)] = {
      (0 to pancakes._1.length - spatula)
        .map(i => newPancakes(pancakes._1, i))
        .flatten
        .map((_, pancakes._2 + 1))
        .toStream
    }

    val results = breadth_first_traverse((initPancakes, 0), getChildren) find (_._1.reduce(_ && _))
    if (results.isEmpty) return None
    else return Some(results.get._2)
  }

  override def format(output: Output) = output.map(_.toString) getOrElse "IMPOSSIBLE"

  // http://stackoverflow.com/a/41350143
  def breadth_first_traverse[Node](node: Node, f: Node => Seq[Node]): Stream[Node] = {
    def recurse(q: Queue[Node]): Stream[Node] = {
      if (q.isEmpty) {
        Stream.Empty
      } else {
        val (node, tail) = q.dequeue
        node #:: recurse(tail ++ f(node))
      }
    }

    node #:: recurse(Queue.empty ++ f(node))
  }
}
