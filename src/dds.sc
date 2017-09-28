object dds {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet


  def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
    if (halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)
  //> controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S
  //| , Int) => S)S

  val finalPop = controlLoop(1, 0, (p: Int, t: Int) => p >= 10000, (p: Int, t: Int) => 2 * p)
  //> finalPop  : Int = 16384

  finalPop                                        //> res0: Int = 16384


  val delta = 1e-5                                //> delta  : Double = 1.0E-5

  def deriv(f: Double=>Double): Double=>Double = {
    def df(x: Double) = (f(x + delta) - f(x))/delta
    df _
  }                                               //> deriv: (f: Double => Double)Double => Double

  def foo(x: Double) = 3 * x * x                  //> foo: (x: Double)Double
  val dfoo = deriv(foo)                           //> dfoo  : Double => Double = <function1>
  dfoo(10)                                        //> res1: Double = 60.00002999826392

  def solve(f: Double=>Double): Double = {
    def df = deriv(f)
    def goodEnuf(guess: Double, c: Int) = math.abs(f(guess)) <= delta
    def improve(guess: Double, c:Int) = guess - f(guess)/df(guess)
    controlLoop(1.0, 0, goodEnuf,improve)
  }                                               //> solve: (f: Double => Double)Double

  def squareRoot(n: Double) = solve((x: Double) => x * x - n)
  //> squareRoot: (n: Double)Double

  squareRoot(81)                                  //> res2: Double = 9.000000000013383
  squareRoot(49)                                  //> res3: Double = 7.000000142285558


}