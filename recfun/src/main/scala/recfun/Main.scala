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
      if (c == 0 || c == r)
        return 1
      pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def _balance(chars: List[Char], open: Int): Boolean = {
        if (open < 0)
          return false

        if (chars.isEmpty)
          return open == 0

        val c = chars.head
        if (c == '(') {
          _balance(chars.tail, open+1)
        } else if (c == ')') {
          _balance(chars.tail, open-1)
        } else {
          _balance(chars.tail, open)
        }
      }

      _balance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 
        return 0
      if (money == 0)
        return 1
      else if (money < 0)
        return 0
      countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }
