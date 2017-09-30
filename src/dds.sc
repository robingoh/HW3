object dds {
  // problem 1
  def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
    if (halt(state, cycle))
      state
    else
      controlLoop(update(state, cycle), cycle + 1, halt, update)

  // problem 2
  val finalPopulation = controlLoop(
    1, // initial population
    0,
    (current: Int, cycle: Int) => current >= 100000, // halt when exceeds 10^5
    (current: Int, cycle: Int) => 2 * current) // expecting: 131072
  //***********

  // problem 3
  // Finding Roots of Functions with Newton's method
  // def solve(f: Double=>Double) = r where |f﴾r﴿| <= delta
  // guess = guess ­ f(guess)/f'(guess)
  val delta = 1e-9
  def derivative(f: Double=>Double): Double=>Double = {
    def deriv(x: Double) = (f(x + delta) - f(x)) / delta
    deriv
  }
  def solve(f: Double=>Double): Double = {
    def firstDerivative = derivative(f)
    def halt(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
    def update(guess: Double, cycle: Int) = guess - f(guess)/firstDerivative(guess)
    controlLoop(1.0, 0, halt, update)
  }
  // tests:
  def f1(x: Double) = x * x + x - 12
  solve(f1) // expecting: approx. 3
  def f2(x: Double) = 4 * x * x - 1
  solve(f2) // expecting: approx. 0.5
  //***********

  // problem 4
  // A function that approximates square root
  def squareRoot(x: Double) = solve((n: Double) => n * n - x)
  // tests:
    squareRoot(0) // expecting: approx. 0
    squareRoot(81) // expecting: approx. 9
    squareRoot(49) // expecting: approx. 7
    squareRoot(9) // expecting: approx. 3
    squareRoot(16) // expecting: approx. 4
    squareRoot(2) // expecting: approx. 1.414
    squareRoot(3) // expecting: approx. 1.732
  //***********

  // problem 5
  // A function that approximate cube roots
  def cubeRoot(x: Double) = solve((n: Double) => n * n * n - x)
  // tests:
  for (i <- -10 to 10) {
    val cubedNumber = math.pow(i, 3)
    println(cubeRoot(cubedNumber))
    // expecting(all approx.): -10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10
  }
  //***********

  // problem 6
  // A function that approximate nth roots
  def nthRoot(x: Double, n: Int) = solve((num: Double) => math.pow(num, n) - x)
  // tests:
  nthRoot(4, 2) // expecting: 2
  nthRoot(27, 3) // expecting: 3
  nthRoot(256, 4) // expecting: 4
  nthRoot(32, 5) // expecting: 2
  nthRoot(64, 6) // expecting: 2
  nthRoot(128, 7) // expecting: 2
  nthRoot(256, 8) // expecting: 2
}