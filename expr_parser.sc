import $file.parser
import parser.Syntax._
import parser._

/*
   grammar:

   expr ::= term (+ expr | \epsilon) \
   term ::= factor (* term | \epsilon) \
   factor ::= (expr) | nat \
   nat ::= 0 | 1 | 2 | \dots \
 */

/** Parses an expression according to:
  *
  * expr ::= term (+ expr | \epsilon)
  *
  * Ignores spaces around tokens.
  *
  * Example:
  *
  * {{{
  *   expr("2*(3+4)") == Some((14, ""))
  * }}}
  *
  */
lazy val expr: Parser[Int] = fail

/** Parses terms according to:
  *
  * term ::= factor (* term | \epsilon)
  *
  */
lazy val term: Parser[Int] = fail

/** Parses factors according to:
  *
  * factor ::= (expr) | nat
  * nat ::= 0 | 1 | 2 | ...
  *
  */
lazy val factor: Parser[Int] = fail

/** Evaluates an input string to its integer representation.
  *
  * Returns an `Int` if the string can be parsed and evaluated successfully without any remaining unconsumed string.
  * Otherwise it returns an error message.
  *
  * @param input input string
  * @return a result that is either a `String` or an `Int`
  */
def eval(input: String): Either[String, Int] =
  expr(input) match {
    case Some((n, ""))  => Right(n)
    case Some((_, out)) => Left(s"unconsumed input: $out")
    case None           => Left("invalid input")
  }
