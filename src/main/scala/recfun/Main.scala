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
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def loop(col:Float, acc: Float): Int = {
      if (col < c ) {
        if(col == 0) loop(col + 1,acc + 1)
        else {
        loop(col + 1, ((r.toFloat + 1 - col) / col) * acc)
        }
      } else (((r.toFloat + 1 - col)/col) * acc).toInt
    }
    if(c == r | c == 0) 1
    else loop(0, 0)
  }

  def balance(chars: List[Char]): Boolean = {

    val open = "(".toList(0)
    val close = ")".toList(0)


    def countBalanced(sample: List[Char], count: Int): Int = {
      def countClosed(rear:List[Char], count:Int):Int = {
        rear match {
          case h :: t if h == close => count + 1
          case _ :: t => countClosed(t, count)
          case Nil => count
        }
      }
      sample match {
        case h :: t if h == open => countBalanced(t, countClosed(t, count))
        case _ :: t => countBalanced(t, count)
        case Nil => count
        }
      }


    def countParen(parenthesis:List[Char], count:Int):Int = {
      parenthesis match {
        case h :: t if (h == open) | (h == close) => countParen(t, count + 1)
        case _ :: t => countParen(t, count)
        case Nil => count
      }
    }

      countBalanced(chars,0) == (countParen(chars,0).toFloat /2)
  }

  /**
   * Exercise 3
   */
 
  def countChange(money:Int, coins:List[Int]):Int = {
    def recursiveCount(a:Int, coinPerm:List[Int]):Int ={
      if( a == 0) 1
      else if(a < 0 | coinPerm.isEmpty) 0
      else recursiveCount(a, coinPerm.tail) + recursiveCount(a - coinPerm.head, coinPerm)
    }
    recursiveCount(money,coins)
  }

}



