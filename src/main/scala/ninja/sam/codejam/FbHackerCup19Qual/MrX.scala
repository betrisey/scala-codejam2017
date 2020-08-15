package ninja.sam.codejam.FbHackerCup19Qual

import ninja.sam.codejam.Solver

object MrX extends Solver(inputFile = "mr_x_sample_input.txt") {
  override type Input = String
  override type Output = Int

  override def read = {
    in.next[String]
  }

  override def apply(input: Input): Output = {
    val b1 = LogicalExpression(Map("x" -> true, "X" -> false)).sparse(input).get
    val b2 = LogicalExpression(Map("x" -> false, "X" -> true)).sparse(input).get

    return if (b1 == b2) 0 else 1
  }

  override def format(output: Output) = output.toString
}

import scala.util.parsing.combinator.JavaTokenParsers
case class LogicalExpression(variableMap: Map[String, Boolean]) extends JavaTokenParsers {
  private lazy val b_expression: Parser[Boolean] = b_xor ~ rep("|" ~ b_xor) ^^ { case f1 ~ fs ⇒ (f1 /: fs)(_ || _._2) }
  private lazy val b_xor: Parser[Boolean] = b_term ~ rep("^" ~ b_term) ^^ { case f1 ~ fs ⇒ (f1 /: fs)(_ ^ _._2) }
  private lazy val b_term: Parser[Boolean] = (b_not_factor ~ rep("&" ~ b_not_factor)) ^^ { case f1 ~ fs ⇒ (f1 /: fs)(_ && _._2) }
  private lazy val b_not_factor: Parser[Boolean] = opt("!") ~ b_factor ^^ (x ⇒ x match { case Some(v) ~ f ⇒ !f; case None ~ f ⇒ f })
  private lazy val b_factor: Parser[Boolean] = b_literal | b_variable | ("(" ~ b_expression ~ ")" ^^ { case "(" ~ exp ~ ")" ⇒ exp })
  private lazy val b_literal: Parser[Boolean] = "1" ^^ (x ⇒ true) | "0" ^^ (x ⇒ false)
  // This will construct the list of variables for this parser
  private lazy val b_variable: Parser[Boolean] = variableMap.keysIterator.map(Parser(_)).reduceLeft(_ | _) ^^ (x ⇒ variableMap(x))

  def sparse(expression: String) = this.parseAll(b_expression, expression)
}
