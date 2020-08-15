package ninja.sam.codejam.FbHackerCup19R1

import better.files._

object ClassTreasurer extends App {
  val inputFile = "class_treasurer_sample_input.txt"
  type Input = (Int, List[Boolean])
  type Output = Int

  def read = {
    var n = in.next[Int]
    val k = in.next[Int]

    val votes = in.next().map(_ == 'A').toList
    (k, votes)
  }

  def apply(input: Input): Output = {
    val (k, votes) = input
    def processDup[T](ls : List[Boolean], reduce: List[Boolean] => List[T]) : List[T] = {
      def iter (lst : List[Boolean]): List[T] ={
        lst match {
          case head::tail => {
            val (duplst,remainlst) = lst.span(_ == head)
            reduce(duplst) ::: iter(remainlst) }
          case Nil => List[T]()
        }
      }
      iter(ls)
    }

    def runLength(ls : List[Boolean]) : List[(Int, Boolean)] ={
      List((ls.size,ls.head))
    }

    val count = processDup(votes, runLength)

    val toPay = scala.collection.mutable.HashMap.empty[Int, List[Int]]
    val subsets = count.zipWithIndex

    for (subset <- subsets.filter(x => x._1._2 == false && x._1._1 > k)) {
      val list = (subset._1._1 to 0 by -(k + 1)).toList.tail.reverse
      toPay += (subset._2 -> list)
    }

    var index = 0
    var amount = 0
    val mod = BigInt(1000000007)
    for (subset <- subsets) {
      if (toPay.contains(subset._2)) {
        for (i <- toPay(subset._2)) {
          val x = BigInt(2).modPow(BigInt(1 + index + i), mod)
          amount = (amount + x.toInt) % 1000000007
        }
      }
      index += subset._1._1
    }

    amount
  }

  def format(output: Output) = output.toString

  /* Template */
  val in = Scanner(getClass.getResourceAsStream(inputFile))(Scanner.Config.default)
  val out = File(inputFile.replace("_input.txt", "_output.txt")).newOutputStream.printer()

  println(s"Solving: $inputFile")
  for (i <- 1 to in.next[Int]) {
    val output = s"Case #$i: ${format(apply(read))}"
    println(output)
    out.println(output)
  }
  in.close()
  out.close()
}
