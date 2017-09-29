object FunctionalProgramming {
  // problem 1
  // a generic compose combinator
  def compose[T](f: T=>T, g: T=>T): T => T = {
    def h(x: T): T = f(g(x))
    h _
  }
  // tests:
  def plus2(x: Double) = x + 2
  def mul3(x: Double) = x * 3
  def mul3ThenPlus2 = compose(plus2 _, mul3 _)
  mul3ThenPlus2(3)
  def plus2ThenMul3 = compose(mul3 _, plus2 _)
  plus2ThenMul3(4)
  //*********************

  // problem 2
  // a self-composition iterator combinator
  def selfIter[T](f: T=>T, n: Int): T=>T = {
    // f composed with itself n times
    if (n == 0)
      f
    else
      compose(selfIter(f, n-1), f)
  }
  // tests:
  def inc(x: Double) = x + 1
  def double(x: Double) = 2 * x
  selfIter(inc _, 10)(3)
  selfIter(double _, 4)(2)
  //*********************

  // problem 3
  // counts the number of elements in an array of elements
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
  countPass(testArray, oddNumberTest _)
  //*********************

  // problem 4
  // An implementation of a recursive combinator that
  // that takes two input and return a recursive function
  def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
    def func(n: Int): Int = {
      if (n == 0)
        baseVal
      else
        combiner(n, func(n-1))
    }
    func _
  }
  def factorial = recur(1, (x: Int, y: Int) => x * y)
  // tests:
  for (i <- 0 to 10)
    println(factorial(i))
  //*********************

  // problem 5
  /* Some programmers like to handle errors by throwing exceptions,
  others like to return the optional value None. Implement a combinator
  called deOptionize that takes a unary, option-returning function, f,
  as input and converts it into a non-option returning function g that
  handles errors by throwing exceptions.
  Use your combinator to convert the following function:


   */

  def deOptionize[T, E](f: T=>Option[E]): T=>E = {
    def g(input: T): E = {
      f(input) match {
        case None => throw new Exception("The input is incorrect.")
        case Some(x) => x
      }
    }
    g _
  }
  // tests:
  def parseDigits(digits: String): Option[Int] =
    if (digits.matches("[0-9]*")) Some(digits.toInt) else None
  val parseDigitsNonOption = deOptionize(parseDigits _)
  parseDigitsNonOption("5437")
  try {
    parseDigitsNonOption("blabla")
  } catch {
    case e: Exception => println(e)
  }



















}