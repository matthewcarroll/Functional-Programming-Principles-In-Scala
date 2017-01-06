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
      if (r == 0 || c == 0 || c == r) 1
      else if (c > r) 0
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      balance(chars, 0)
    }

    private def balance(chars: List[Char], unbalancedCount: Int): Boolean = {
      if (chars.isEmpty) unbalancedCount == 0
      else if (unbalancedCount < 0) false
      else {
        val count =
          if (chars.head == '(') unbalancedCount + 1
          else if (chars.head == ')') unbalancedCount - 1
          else unbalancedCount
        balance(chars.tail, count)
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money <= 0) 0
      else countChangeAux(money, coins.sorted.reverse)
    }

  private def countChangeAux(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChangeAux(money - coins.head, coins) + countChangeAux(money, coins.tail)
    }
  }

