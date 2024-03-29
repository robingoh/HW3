object FunctionalProgramming {
  // problem 1
  // A generic compose combinator
  def compose[T](f: T=>T, g: T=>T): T => T = {
    def h(x: T): T = f(g(x))
    h
  }
  // tests:
  def plus2(x: Double) = x + 2
  def mul3(x: Double) = x * 3
  def mul3ThenPlus2 = compose(plus2, mul3)
  mul3ThenPlus2(3) // expecting: 11.0
  def plus2ThenMul3 = compose(mul3, plus2)
  plus2ThenMul3(4) // expecting: 18.0
  //*********************

  // problem 2
  // A self-composition iterator combinator
  def selfIter[T](f: T=>T, n: Int): T=>T = {
    // f composed with itself n times
    if (n == 0)
      (x: T) => x
    else if (n == 1)
      f
    else
      compose(selfIter(f, n-1), f)
  }
  // instructor's provided functions:
  def inc(x: Double) = x + 1
  def double(x: Double) = 2 * x
  // tests:
  selfIter(inc, 10)(3) // expecting: 13.0
  selfIter(inc, 0)(4) // expecting: 4.0
  selfIter(double, 5)(3) // expecting: 96.0
  selfIter(double, 0)(14) // expecting: 14.0
  //*********************

  // problem 3
  // A function that counts the number of elements in an array of elements
  // of type T that pass a test of type T=>Boolean
  def countPass[T](array: Array[T], test: T=>Boolean) = {
    var numberOfElements = 0
    for (i <- array)
      if (test(i))
        numberOfElements += 1
    numberOfElements
  }
  // tests:
  val testArray = Array(2, 18, 16, 9, 15, 3, 13, 5, 14, 12, 1, 4, 6, 8, 11, 17, 7, 10)
  def oddNumberTest(n: Int) = if (n % 2 != 0) true else false
  countPass(testArray, oddNumberTest) // expecting: 9
  //*********************

  // problem 4
  // An implementation of a recursive combinator that
  // that takes two input and return a recursive function
  // Part A:
  def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
    def func(n: Int): Int = {
      if (n == 0)
        baseVal
      else
        combiner(n, func(n-1))
    }
    func
  }
  // tests for Part A:
  recur(0, (x: Int, y: Int) => x + y)(5) // expecting: 15
  // Part B:
  def factorial = recur(1, (x: Int, y: Int) => x * y)
  // tests:
  for (i <- 0 to 10)
    println(factorial(i)) // expecting: 1,1,2,6,24,120,720,5040,40320,362880,3628800
  //*********************

  // problem 5
  // A combinator that takes a a unary, option-returning function, f,
  // as input and converts it into a non-option returning function g that
  // handles errors by throwing exceptions.
    def deOptionize[T, E](f: T=>Option[E]): T=>E = {
    def g(input: T): E = {
      f(input) match {
        case None => throw new Exception("The input is incorrect.")
        case Some(x) => x
      }
    }
    g
  }
  // tests:
  def parseDigits(digits: String): Option[Int] =
    if (digits.matches("[0-9]*")) Some(digits.toInt) else None
  val parseDigitsNonOption = deOptionize(parseDigits)
  parseDigitsNonOption("5437") // expecting: 5437 of type Int
  try {
    parseDigitsNonOption("blabla") // expecting: exception thrown
  } catch {
    case e: Exception => println(e)
  }
}