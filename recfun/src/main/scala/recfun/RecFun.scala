package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c, r) match
    case (0, _)           => 1
    case (a, b) if a == b => 1
    case (_, _)           => pascal(c - 1, r - 1) + pascal(c, r - 1)
  

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    def balance_count(chars: List[Char], count: Int): Boolean =
      if count < 0 then false
      else if chars.isEmpty && count == 0 then true
      else if chars.isEmpty && count > 0 then false
      else (chars.head, chars.tail) match 
        case ('(', t) => balance_count(t, count + 1)
        case (')', t) => balance_count(t, count - 1)
        case (h, t)   => balance_count(t, count)
      
    balance_count(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then 1
    else if money < 0 || coins.isEmpty then 0
    else (coins.head, coins.tail) match
        case (h, t) =>
            countChange(money, t) + countChange(money - h, coins)
        