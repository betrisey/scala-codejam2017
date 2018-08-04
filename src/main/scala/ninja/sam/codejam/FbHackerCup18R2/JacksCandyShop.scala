package ninja.sam.codejam.FbHackerCup18R2

import better.files._

object JacksCandyShop extends App {
  val inputFile = "jacks_candy_shop_final.in"
  type Input = (Array[Int], Array[Int])
  type Output = Long

  def read: Input = {
    val n = in.next[Int]
    val m = in.next[Int]
    val a = in.next[Int]
    val b = in.next[Int]


    val candiesParent = new Array[Int](n)
    candiesParent(0) = -1
    for (i <- 1 until n)
      candiesParent(i) = in.next[Int]

    def c(i: Int): Int = (a * i + b) % n
    val customers = new Array[Int](n)
    for (i <- 0 until m) {
      customers(c(i)) += 1
    }

    (candiesParent, customers)
  }

  def apply(input: Input): Output = {
    val candies = input._1
    val customers = input._2

    var total: Long = 0
    // try to sell the candies starting with the most expensive
    for(i <- candies.length-1 to 0 by -1) {
      if (customers(i) > 0) {
        // We have a customer for this candy
        total += i
        customers(i) -= 1
      } else {
        // Look for customer in the parents
        var parent = candies(i)
        var sold = false
        while (parent != -1 && !sold) {
          if (customers(parent) > 0) {
            total += i
            customers(parent) -= 1
            sold = true
          }
          parent = candies(parent)
        }
      }
    }

    return total
  }

  def format(output: Output): String = output.toString

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
