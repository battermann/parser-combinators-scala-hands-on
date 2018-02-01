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
lazy val expr: Parser[Int] =
  for {
    t <- term
    res <- (for {
              _ <- symbol("+")
              e <- expr
            } yield t + e) +++ succeed(t)
  } yield res

/** Parses terms according to:
  *
  * term ::= factor (* term | \epsilon)
  *
  */
lazy val term: Parser[Int] =
  for {
    f <- factor
    res <- (for {
              _ <- symbol("*")
              t <- term
            } yield f * t) +++ succeed(f)
  } yield res

/** Parses factors according to:
  *
  * factor ::= (expr) | nat
  * nat ::= 0 | 1 | 2 | ...
  *
  */
lazy val factor: Parser[Int] =
  (for {
    _ <- symbol("(")
    e <- expr
    _ <- symbol(")")
  } yield e) +++ natural

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
