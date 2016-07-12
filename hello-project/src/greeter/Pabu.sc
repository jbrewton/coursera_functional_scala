package greeter

object Pabu {
  def countChange(money: Int, coins: List[Int]): Int = {
	  
	  if (money == 0) {
	    1
	  } else if (money > 0 && !coins.isEmpty) {
	    // give change for the leading coin, give change for the remaining coins
	    countChange(money - coins.head, coins) + countChange(money, coins.tail)
	  } else {
	    0
	  }
	}                                         //> countChange: (money: Int, coins: List[Int])Int
	countChange(4000,List(1,2))               //> res0: Int = 2001
	
	def countChange2(money: Int, coins: List[Int]): Int = {
	  def internalCountChange(acc: Int, money: Int, coins: List[Int]): Int = {
		  
		  
		  
		  if (money == 0) {
		    acc + 1
		  } else if (money > 0 && !coins.isEmpty) {
		    // give change for the leading coin, give change for the remaining coins
		    internalCountChange(acc, money - coins.head, coins) + internalCountChange(acc, money, coins.tail)
		  } else {
		    acc
		  }
		  
		  
	  }
	  internalCountChange(0, money, coins)
	}                                         //> countChange2: (money: Int, coins: List[Int])Int
	countChange2(4000, List(1,2))             //> res1: Int = 2001
}