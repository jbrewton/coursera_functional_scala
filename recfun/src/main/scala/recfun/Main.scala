package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
            
      // beginning and ends are 1
      if ( c == 0 || c == r ) {
        1
      } else {
        // internal values are the previous rows sum of current and prior
        pascal(c-1, r-1) + pascal(c, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def innerBalance(acc: Int, idx: Int): Boolean = {
        
        //if we are at the end or there is a close before an open then we escape
        if (idx == chars.length || acc < 0) {
          return acc == 0
        }
        
        //count up when we open, count down when we close
        if (chars(idx) == '(') {
          innerBalance(acc+1, idx+1)
        } else if (chars(idx) == ')' ) {
          innerBalance(acc-1, idx+1)
        } else {
          innerBalance(acc, idx+1)
        }
      }
      
      innerBalance(0, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      
      if (money == 0) {
        1
      } else if (money > 0 && !coins.isEmpty) {
        // give change for the leading coin, give change for the remaining coins
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      } else {
        0
      }
    }
  }
