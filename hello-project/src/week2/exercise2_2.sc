package week2

object exercise2_2 {
  def product(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) 1
  	else f(a) * product(f)(a + 1, b)          //> product: (f: Int => Int)(a: Int, b: Int)Int
  
  product(x=>x*x)(3,4)                            //> res0: Int = 144
  
  def fact(n: Int) = product(x => x)(1, n)        //> fact: (n: Int)Int
  fact(5)                                         //> res1: Int = 120
  
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  	if (a > b) zero
  	else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
  def productMR(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x * y, 1)(a, b)
                                                  //> productMR: (f: Int => Int)(a: Int, b: Int)Int
  
  productMR(x=>x*x)(3,4)                          //> res2: Int = 144
  
  def factMR(n: Int) = productMR(x => x)(1, n)    //> factMR: (n: Int)Int
  
  factMR(5)                                       //> res3: Int = 120
  	
}