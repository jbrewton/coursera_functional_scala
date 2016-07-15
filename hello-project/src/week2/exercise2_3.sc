package week2
import math.abs

object exercise2_3 {
  // finding fixed points
  
  val tolerance = 0.0001                          //> tolerance  : Double = 1.0E-4
  def isCloseEnough(x: Double, y: Double) =
  	abs((x - y) / x) / x < tolerance          //> isCloseEnough: (x: Double, y: Double)Boolean
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate(guess: Double): Double = {
  		println("guess = " + guess)
  		val next = f(guess)
  		if (isCloseEnough(guess, next)) next
  		else iterate(next)
  	}
  	
  	iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  fixedPoint(x => 1 + x/2)(1)                     //> guess = 1.0
                                                  //| guess = 1.5
                                                  //| guess = 1.75
                                                  //| guess = 1.875
                                                  //| guess = 1.9375
                                                  //| guess = 1.96875
                                                  //| guess = 1.984375
                                                  //| guess = 1.9921875
                                                  //| guess = 1.99609375
                                                  //| guess = 1.998046875
                                                  //| guess = 1.9990234375
                                                  //| guess = 1.99951171875
                                                  //| res0: Double = 1.999755859375
  
  
  def sqrt(x: Double) = fixedPoint(y => (y + x/y) /2)(1)
                                                  //> sqrt: (x: Double)Double
  	
  sqrt(2)                                         //> guess = 1.0
                                                  //| guess = 1.5
                                                  //| guess = 1.4166666666666665
                                                  //| guess = 1.4142156862745097
                                                  //| res1: Double = 1.4142135623746899
                                                  
	def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
                                                  
	def sqrtD(x: Double) = fixedPoint(averageDamp(y => x/y))(1)
                                                  //> sqrtD: (x: Double)Double
	sqrtD(2)                                  //> guess = 1.0
                                                  //| guess = 1.5
                                                  //| guess = 1.4166666666666665
                                                  //| guess = 1.4142156862745097
                                                  //| res2: Double = 1.4142135623746899
	sqrtD(16)                                 //> guess = 1.0
                                                  //| guess = 8.5
                                                  //| guess = 5.1911764705882355
                                                  //| guess = 4.136664722546242
                                                  //| guess = 4.002257524798522
                                                  //| guess = 4.000000636692939
                                                  //| res3: Double = 4.000000000000051
}