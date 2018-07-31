package ninja.sam.codejam.FbHackerCup18R1

import better.files._

object EthanTraversesTree extends App {
  val inputFile = "ethan_traverses_a_tree_final.in"
  type Input = (Int, Int, Node)
  type Output = Option[Array[Int]]

  def read = {
    var n = in.next[Int]
    val k = in.next[Int]

    val treeArray = new Array[(Int, Int)](n).map(_ => (in.next[Int], in.next[Int]))


    def buildTree(i: Int): Node = {
      val (a, b) = treeArray(i - 1)

      new Node(if (a == 0) None else Some(buildTree(a)),
        if (b == 0) None else Some(buildTree(b)),
        i)
    }

    (n, k, buildTree(1))
  }

  def apply(input: Input): Output = {
    val n = input._1
    val k = input._2
    val tree = input._3

    def preOrder(node: Option[Node]): List[Int] = {
      node match {
        case Some(n) => n.label :: preOrder(n.left) ::: preOrder(n.right)
        case None => Nil
      }
    }

    def postOrder(node: Option[Node]): List[Int] = {
      node match {
        case Some(n) => postOrder(n.left) ::: (postOrder(n.right) :+ n.label)
        case None => Nil
      }
    }

    val preArray = preOrder(Some(tree)).toArray
    val postArray = postOrder(Some(tree)).toArray

    def assignLabel(answer: Array[Int], label: Int): Array[Int] = {
      var i = answer.indexOf(0) // first unassigned node
      if (i == -1) // all nodes assigned
        return answer

      def updateRecursively(answer: Array[Int], i: Int, startNode: Int, label: Int): Array[Int] = {
        val postNode = postArray(i)
        if (postNode == startNode) return answer
        val index = preArray.indexOf(postNode)
        return updateRecursively(answer.updated(index, label), index, startNode, label)
      }

      val nextLabel = if (label < k) label + 1 else k
      return assignLabel(updateRecursively(answer.updated(i, label), i, preArray(i), label), nextLabel)
    }

    val answer = assignLabel(new Array[Int](n), 1)
      .zip(preArray).sortBy(_._2).map(_._1) // results are the new labels for the pre-order array, so we have to sort it

    val max = answer.max
    if (max == k)
      return Some(answer)
    return None
  }

  case class Node(left: Option[Node], right: Option[Node], label: Int)

  def format(output: Output) = output.map(_.mkString(" ")).getOrElse("Impossible")

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
