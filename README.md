# A Hands On Functional Parsers In Scala

> A hands on functional parsers in Scala based on "Programming in Haskell" by Graham Hutton.

## Prerequisites

- Install [Ammonite](https://ammonite.io/#ScalaScripts)

## How To

This repo has two branches, `exercises` and `solutions`.

Checkout the `exercises` branch and run the tests with `amm -w test.sc`.

You should get an output similar to this:

```text
tests$ParserTests:
- fail should always fail
- succeed should always succeed with the result value v (pending)
- anyChar should succeed with first char for non empty inputs (pending)
- anyChar should fail if input is empty (pending)
- sequencing (pending)
…
```

Now, take look at the tests in `tests.sc`. Here is an example:

```scala
property("fail should always fail") {
  pending
  forAll { (input: String) =>
    parser.fail(input) should ===(None)
  }
}
```

- Go from top to bottom
- Remove the line which says `pending` from the first pending test
- This should usually make the test fail (red)
- Now implement the function under test (in `parser.sc` and `expr_parser.sc`) and try to make the test pass (green)
- Think about if you can improve your implementation (refactor)
- Work through the tests from top to bottom until all tests pass
