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


    def pascal(c: Int, r: Int): Int = (c, r) match {
     case (0, _) => 1
     case (c, r) if (c == r) => 1
     case (_, _) => pascal(c - 1, r - 1) + pascal(c, r - 1)
   }


  /**
    * alternatively with if, else
    * def pascal(c: Int, r: Int): Int =
    *  if(c == 0 || c == r) 1
    *  else pascal(c - 1, r - 1) + pascal(c, r - 1)
    *
    *
    * }
      if(c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    */

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanceLoop(acc: Int, rest: List[Char]): Boolean = (acc, rest) match {
        case (acc, rest) if(rest.isEmpty) => acc == 0
        case (acc, rest) => rest.head match {
          case '(' => balanceLoop(acc+1, rest.tail)
          case ')' if(acc == 0) => false
          case ')' => balanceLoop(acc-1, rest.tail)
          case _ => balanceLoop(acc, rest.tail)
        }
      }

      balanceLoop(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
      case(money, _) if(money < 0) => 0
      case(_, coins) if(coins.isEmpty) => 0
      case(0, _) => 1
      case(_, _) => countChange(money - coins.head, coins) + countChange(money, coins.tail)

    }
  }
