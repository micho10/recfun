package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parenthesis balance")

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || c > r) 0
    else if (r == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(count: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) count == 0
      else if (chars.head != '(' && chars.head != ')')
        innerBalance(count, chars.tail)
      else {
        val aux = if (chars.head == '(') count + 1 else count - 1
        aux >= 0 && innerBalance(aux, chars.tail)
      }
    }

    innerBalance(0, chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
