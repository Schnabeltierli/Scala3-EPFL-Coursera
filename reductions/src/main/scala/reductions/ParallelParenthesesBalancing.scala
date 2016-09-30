package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def braketCounter(chars: Array[Char], counter: Int): Int = {
      if (chars.isEmpty || counter < 0) counter
      else if (chars.head == '(') braketCounter(chars.tail, counter + 1)
      else if (chars.head == ')') braketCounter(chars.tail, counter - 1)
      else braketCounter(chars.tail, counter)
    }
    braketCounter(chars, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    /** def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      ???
    } */
    def traverse(from: Int, to: Int): (Int, Int) = {
      var left = 0
      var right = 0
      var i = from
      while (i <= to){
        if(chars(i)== "(") left = left + 1
        else if(chars(i)== ")" && left > 0) left = left - 1
        else if(chars(i)== ")") right = right + 1
        i = i + 1
      }
      (left, right)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until-from < threshold) traverse(from, until)
      else {
        val midPoint = (until - from)/2
        val ((l1, r1), (l2, r2)) = parallel(traverse(from, midPoint), traverse(midPoint, until))
        val m = math.min(l1, r2)
        (l1 + l2 - m , r1 + r2 - m)
      }


    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
