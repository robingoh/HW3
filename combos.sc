object combos {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet


  def iterMaker[T](initVal: T, update: (T, Int)=>T): Int => T = {
    def fun(n: Int): T = {
      var result = initVal
      for(count <- 1 to n) result = update(result, count)
      result
    }

    fun _

  }                                               //> iterMaker: [T](initVal: T, update: (T, Int) => T)Int => T

  val tri = iterMaker(0, (x: Int, y: Int) => x + y)
  //> tri  : Int => Int = <function1>

  tri(5)                                          //> res0: Int = 15
  tri(4)                                          //> res1: Int = 10
  tri(3)                                          //> res2: Int = 6

  val fact = iterMaker(1,  (x: Int, y: Int) => x * y)
  //> fact  : Int => Int = <function1>

  fact(5)                                         //> res3: Int = 120
  fact(4)                                         //> res4: Int = 24
  fact(3)                                         //> res5: Int = 6

  val mkWord = iterMaker("0", (x: String, y: Int) => x + y)
  //> mkWord  : Int => String = <function1>

  mkWord(5)                                       //> res6: String = 012345

  def id(x: Int) = x                              //> id: (x: Int)Int
  def compose(f: Int=>Int, g: Int=>Int) = (x: Int) => f(g(x))
  //> compose: (f: Int => Int, g: Int => Int)Int => Int
  def mul2(x: Int) = 2 * x                        //> mul2: (x: Int)Int

  val iterMul2 = iterMaker(id _, (f: Int=>Int, y: Int)=>compose(mul2, f))
  //> iterMul2  : Int => (Int => Int) = <function1>

  val fun = iterMul2(10)                          //> fun  : Int => Int = <function1>

  fun(3)                                          //> res7: Int = 3072

}