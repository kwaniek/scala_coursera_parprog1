package reductions

import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

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
    def isBalanced(currentChars: Array[Char], numberOfOpened: Int): Boolean =
      (currentChars, numberOfOpened) match {
        case (letters, _) if letters.isEmpty => numberOfOpened == 0
        case (letters, counter) if letters.head == '(' => isBalanced(letters.tail, counter + 1)
        case (letters, counter) if letters.head == ')' =>
          if (counter > 0) isBalanced(letters.tail, counter - 1)
          else false
        case (letters, counter) => isBalanced(letters.tail, counter)
      }

    isBalanced(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, minimumNesting: Int, totalGain: Int): (Int, Int) = {
      ???
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold) {

      } else {

      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
