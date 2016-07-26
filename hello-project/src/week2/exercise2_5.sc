object exercise2_5 {
  val x = new Rational(1,3)                       //> x  : Rational#33529 = 1/3
  val y = new Rational(5,7)                       //> y  : Rational#33529 = 5/7
  val z = new Rational(3,2)                       //> z  : Rational#33529 = 3/2
  
  x.numer                                         //> res0: Int#1068 = 1
  x.denom                                         //> res1: Int#1068 = 3
  x.sub(y).sub(z)                                 //> res2: Rational#33529 = -79/42
  y + y                                           //> res3: Rational#33529 = 10/7
  x < y                                           //> res4: Boolean#2475 = true
  x max y                                         //> res5: Rational#33529 = 5/7
	
	new Rational(2)                           //> res6: Rational#33529 = 2/1
}

class Rational(x: Int, y: Int) {
	require(y != 0, "denominator must be nonzero")

	def this(x: Int) = this (x, 1)

	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
	def numer = x
	def denom = y
	
	def < (that: Rational) = numer * that.denom < that.numer * denom
	
	def max(that: Rational) = if (this < that) that else this
	
	def + (that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom
		)
	
	override def toString = {
		val g = gcd(numer,denom)
		numer/g + "/" + denom/g
	}
	
	def unary_- : Rational = new Rational(-numer, denom)
	
	def sub(that: Rational) = this + (-that)
}