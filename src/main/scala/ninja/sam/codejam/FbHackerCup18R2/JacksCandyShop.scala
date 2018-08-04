package ninja.sam.codejam.FbHackerCup18R2

import better.files._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object JacksCandyShop extends App {
  val inputFile = "jacks_candy_shop_final.in"
  type Input = JacksCandyShopInput
  type Output = Long

  def read: Input = {
    val n = in.next[Int]
    val m = in.next[Int]
    val a = in.next[Int]
    val b = in.next[Int]

    val candies = new Array[Candy](n)
    for (i <- 0 until n)
      candies(i) = Candy(i, new ListBuffer[Int]())

    val inputs = (1 until n).map(i => (i, in.next[Int])) // (price, parent)
    for (input <- inputs)
      candies(input._2).children += input._1

    def c(i: Int): Int = (a * i + b) % n
    val customers = (0 until m).map(c).toList

    JacksCandyShopInput(candies, customers)
  }

  def apply(input: Input): Output = {
    val candies = input.candies
    val customers = input.customers

    // TODO: Make if tail recursive
    def candiesForCustomer(c: Int): List[Int] = c +: candies(c).children.flatMap(candiesForCustomer).toList

    val candiesForCustomers = customers.toList.map(candiesForCustomer(_).sorted.reverse)

    @tailrec
    def pickCandy(canForCust: List[List[Int]], acc: Long): Long = {
      if (canForCust.isEmpty)
        return acc

      var custPicked = 0
      var candyPicked = 0

      custPicked = canForCust.zipWithIndex.sortBy(_._1.length).map(_._2).head
      candyPicked = canForCust(custPicked).head

      val newCanForCust = (canForCust.take(custPicked) ++ canForCust.drop(custPicked+1))
        .map(_.filterNot(_ == candyPicked))
        .filter(_.nonEmpty)
      return pickCandy(newCanForCust, acc + candyPicked.toLong)
    }

    return pickCandy(candiesForCustomers, 0)
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

case class JacksCandyShopInput(candies: Array[Candy], customers: List[Int])
case class Candy(price: Int, children: ListBuffer[Int])