import $ivy.`org.scalactic::scalactic:3.1.1`
import $ivy.`org.scalacheck::scalacheck:1.14.1`
import $ivy.`org.scalatestplus::scalacheck-1-14:3.1.0.0`
import $ivy.`org.scalactic::scalactic:3.3.0-SNAP2`

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import $file.parser
import $file.expr_parser

class ParserTests extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with TypeCheckedTripleEquals {
  import parser.Syntax._
  import parser._

  property("fail should always fail") {
    forAll { (input: String) =>
      parser.fail(input) should ===(None)
    }
  }

  property("succeed should always succeed with the result value v") {
    pending
    forAll { (input: String, v: Int) =>
      parser.succeed(v)(input) should ===(Some((v, input)))
    }
  }

  property("anyChar should succeed with first char for non empty inputs") {
    pending
    forAll { (input: String) =>
      whenever(input.nonEmpty)(
        parser.anyChar(input) should ===(Some((input.head, input.tail)))
      )
    }
  }

  property("anyChar should fail if input is empty") {
    pending
    parser.anyChar("") should ===(None)
  }

  property("sequencing") {
    pending
    forAll { (c1: Char, c2: Char, c3: Char, out: String) =>
      val input = s"$c1$c2$c3$out"
      val p = for {
        v1 <- parser.anyChar
        _  <- parser.anyChar
        v2 <- parser.anyChar
      } yield s"$v1$v2"

      p(input) should ===(Some((s"$c1$c3", out)))
    }
  }

  property("choice") {
    pending
    forAll { (input: String) =>
      val p1 = parser.fail +++ parser.succeed(42)
      val p2 = parser.succeed(42) +++ parser.fail
      p1(input) should ===(Some((42, input)))
      p2(input) should ===(Some((42, input)))
    }
  }

  property("digit should succeed on numeric char") {
    pending
    forAll(Gen.numChar, Gen.alphaNumStr) { (c, out) =>
      digit(s"$c$out") should ===(Some((c, out)))
    }
  }

  property("digit should fail on alpha char") {
    pending
    forAll(Gen.alphaChar, Gen.alphaNumStr) { (c, out) =>
      digit(s"$c$out") should ===(None)
    }
  }

  property("lower should succeed on lower case char") {
    pending
    forAll(Gen.alphaLowerChar, Gen.alphaNumStr) { (c, out) =>
      lower(s"$c$out") should ===(Some((c, out)))
    }
  }

  property("lower should fail on upper case char") {
    pending
    forAll(Gen.alphaUpperChar, Gen.alphaNumStr) { (c, out) =>
      lower(s"$c$out") should ===(None)
    }
  }

  property("upper should succeed on upper case char") {
    pending
    forAll(Gen.alphaUpperChar, Gen.alphaNumStr) { (c, out) =>
      upper(s"$c$out") should ===(Some((c, out)))
    }
  }

  property("upper should fail on lower case char") {
    pending
    forAll(Gen.alphaLowerChar, Gen.alphaNumStr) { (c, out) =>
      upper(s"$c$out") should ===(None)
    }
  }

  property("letter should succeed on alpha char") {
    pending
    forAll(Gen.alphaChar, Gen.alphaNumStr) { (c, out) =>
      letter(s"$c$out") should ===(Some((c, out)))
    }
  }

  property("alphaNum should succeed on alphaNum char") {
    pending
    forAll(Gen.alphaNumChar, Gen.alphaNumStr) { (c, out) =>
      alphaNum(s"$c$out") should ===(Some((c, out)))
    }
  }

  val noLetterOrDigitChar: Gen[Char] = Gen.oneOf((0 to 20000).map(_.toChar).filterNot(_.isLetterOrDigit))

  property("alphaNum should fail on non-alphaNum char") {
    pending
    forAll(noLetterOrDigitChar, Gen.alphaNumStr) { (c, out) =>
      alphaNum(s"$c$out") should ===(None)
    }
  }

  property("char should succeed if input starts with specified char") {
    pending
    forAll { (c: Char, out: String) =>
      char(c)(s"$c$out") should ===(Some((c, out)))
    }
  }

  property("char should fail if input does not starts with specified char") {
    pending
    forAll { (input: String) =>
      whenever(input.nonEmpty)(
        char((input.head.toInt + 1).toChar)(input) should ===(None)
      )
    }
  }

  property("string should succeed if input starts with specified string") {
    pending
    forAll { (str: String, out: String) =>
      val input = s"$str$out"
      string(str)(input) should ===(Some((str, out)))
    }
  }

  property("string should fail if input does not start with specified string") {
    pending
    forAll { (str: String, input: String) =>
      whenever(!input.startsWith(str)) {
        string(str)(input) should ===(None)
      }
    }
  }

  property("many should always succeed parsing a single char") {
    pending
    forAll { (c: Char, input: String) =>
      many(char(c))(input) should ===(Some((input.toList.takeWhile(_ == c), input.dropWhile(_ == c))))
    }
  }

  property("many should always succeed") {
    pending
    forAll(Gen.oneOf(anyChar, lower, upper, letter, char('f')), arbitrary[String]) { (p, input) =>
      many(p)(input).isDefined should ===(true)
    }
  }

  property("many1 should succeed with 'List(a, b, c)' for input 'abcFoo'") {
    pending
    many1(lower)("abcFoo") should ===(Some((List('a', 'b', 'c'), "Foo")))
  }

  property("many1 should fail for empty inputs") {
    pending
    forAll(Gen.oneOf(anyChar, lower, upper, letter, char('f'))) { p =>
      many1(p)("") should ===(None)
    }
  }

  property("nat should parse natural number") {
    pending
    forAll { (num: Int, out: String) =>
      whenever(num != Int.MinValue && out.headOption.exists(c => !c.isDigit)) {
        nat(s"${Math.abs(num)}$out") should ===(Some((Math.abs(num), out)))
      }
    }
  }

  property("nat max value") {
    pending
    val input = "2147483647"
    nat(input) should ===(Some(2147483647, ""))
  }

  property("nat > max value should fail") {
    pending
    val input = "2147483648"
    nat(input) should ===(None)
  }

  property("nat should fail on non numeric string") {
    pending
    forAll(Gen.alphaStr) { alphaStr =>
      nat(alphaStr) should ===(None)
    }
  }

  property("space should succeed on white spaces tabs and newlines") {
    pending
    space("   foo") should ===(Some(((), "foo")))
    space("   \t foo") should ===(Some(((), "foo")))
    space("   \n foo") should ===(Some(((), "foo")))
  }

  property("space always succeed") {
    pending
    forAll { (input: String) =>
      space(input).isDefined should ===(true)
    }
  }

  property("token should remove spaces") {
    pending
    forAll(Gen.alphaNumStr) { str =>
      token(string(str))(s"   $str   foo") should ===(Some((str, "foo")))
      token(string(str))(s"   ${str}foo") should ===(Some((str, "foo")))
      token(string(str))(s"$str foo") should ===(Some((str, "foo")))
      token(string(str))(s" \t \n $str foo") should ===(Some((str, "foo")))
    }
  }

  property("token should succeed if inner parser succeeds") {
    pending
    forAll { (input: String) =>
      token(parser.succeed(()))(input) should ===(Some((), input))
    }
  }

  property("token should fail if inner parser fails") {
    pending
    forAll { (input: String) =>
      token(parser.fail)(input) should ===(None)
    }
  }

  property("natural should parse a natural number and remove white spaces") {
    pending
    forAll(arbitrary[Int], Gen.alphaStr) { (n: Int, out: String) =>
      whenever(n != Int.MinValue) {
        natural(s"    ${Math.abs(n)}$out") should ===(Some((Math.abs(n), out)))
      }
    }
  }

  property("symbol should parse correct name and remove white spaces") {
    pending
    forAll { (name: String, out: String) =>
      symbol(name)(s"   $name   $out") should ===(Some((name, out)))
    }
  }
}

run(new ParserTests())

class ListParserTests extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with TypeCheckedTripleEquals {
  import parser.Syntax._
  import parser._

  val validLists =
    Table(
      ("input", "int list"),
      ("[1,5,3,5]", List(1, 5, 3, 5)),
      ("  [   4 ,    5,6,7  ,  3 ,   9]", List(4, 5, 6, 7, 3, 9)),
      ("[1]", List(1))
    )

  val invalidLists =
    Table(
      "input",
      "1,5,3,5]",
      "[1,5,3,5",
      "[1 5,3,5]",
      "[]",
      "[,]",
      "[4, 5,]"
    )

  property("nonEmptyIntList should parse valid lists correctly") {
    pending
    forAll(validLists) { (input: String, expected: List[Int]) =>
      val Some((actual, rest)) = nonEmptyIntList(input)
      actual should ===(expected)
      rest should ===("")
    }
  }

  property("nonEmptyIntList should fail with invalid lists") {
    pending
    forAll(invalidLists) { input =>
      nonEmptyIntList(input) should ===(None)
    }
  }
}

run(new ListParserTests())

class ArithmeticExpressionsTests extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with TypeCheckedTripleEquals {
  import expr_parser._

  val validExpressions =
    Table(
      ("expression", "result"),
      ("42", 42),
      ("(((((42)))))", 42),
      ("1+1", 2),
      ("(1+1)", 2),
      ("1*1", 1),
      ("1*2", 2),
      ("(1*2)", 2),
      ("2*3+4", 10),
      ("2*(3+4)", 14),
      ("2 * 3 +  4", 10),
      ("2*(     3+ 4)  ", 14),
      ("2*3", 6),
      ("((1))*(2+(((3)))*(4+(((5))+6))*(((7*8)))+9)", 2531)
    )

  val invalidExpressions =
    Table(
      "expressions",
      "-1",
      "()",
      "(5",
      "(1+2",
      "(1+2()"
    )

  property("eval should evaluate valid expressions correctly") {
    pending
    forAll(validExpressions) { (expr: String, expected: Int) =>
      val actual = eval(expr)
      actual should ===(Right(expected))
    }
  }

  property("eval should fail with invalid expressions") {
    pending
    forAll(invalidExpressions) { input =>
      val actual = eval(input)
      actual should ===(Left("invalid input"))
    }
  }
}

run(new ArithmeticExpressionsTests())
