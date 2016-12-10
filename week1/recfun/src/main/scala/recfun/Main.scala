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
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def countParentheses(chars: List[Char], counter: Int): Boolean = {
      if (chars.isEmpty) counter == 0
      else {
        if (chars.head == ')') counter > 0 && countParentheses(chars.tail, counter - 1)
        else if (chars.head == '(') countParentheses(chars.tail, counter + 1)
        else countParentheses(chars.tail, counter)
      }
    }

    countParentheses(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }
}