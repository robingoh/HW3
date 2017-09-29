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
  selfIter(inc, 10)(3)
  selfIter(double, 4)(2)


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

  // problem 4
  // An implementation of a recursive combinator that
  // that takes two input and return a recursive function
  // A
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
  factorial(5)

}