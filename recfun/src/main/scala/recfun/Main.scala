package recfun
import common._

object Main {
  def main(args: Array[String]) {

    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    //println(balance("(if (zero? x) max (/ 1 x))".toList))

    //println(countChange(4,List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r ) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    if (balance_internal(0, chars) == 0) {true} else {false}

  def balance_internal(sum: Int, chars: List[Char]): Int =
    if (chars.isEmpty)
      sum
    else if (chars.head == '(' && sum >= 0)
      balance_internal(sum + 1, chars.tail)
    else if (chars.head == ')' && sum >= 1)
      balance_internal(sum - 1, chars.tail)
    else if (chars.head == '(' || chars.head == ')')
      -1
    else
      balance_internal(sum, chars.tail)


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0)
      1
    else if (money < 0 || coins.isEmpty)
      0
    else
      countChange_oneCoin(money, coins.sorted.head, coins.sorted.tail)

  def countChange_oneCoin(remain_money: Int, coin: Int, remain_coins: List[Int]): Int =
    if (remain_money - coin == 0)
      1
    else if (remain_money < 0)
      0
    else
      countChange_oneCoin(remain_money - coin, coin, remain_coins) + countChange(remain_money, remain_coins)
}
