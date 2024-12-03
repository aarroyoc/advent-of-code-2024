import scala.io.Source
import scala.util.matching.Regex

object Main:
  def test1: String = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  def test2: String = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("input").mkString
    val result1 = partOne(input)
    println(s"Part 1: $result1")
    val result2 = partTwo(input)
    println(s"Part 2: $result2")

  private def partOne(input: String): Int =
    val mulPattern: Regex = """mul\(([0-9]+),([0-9]+)\)""".r

    var sum = 0

    for patternMatch <- mulPattern.findAllMatchIn(input) do
      val a = patternMatch.group(1).toInt
      val b = patternMatch.group(2).toInt
      sum += a * b
    sum

  private def partTwo(input: String): Int =
    val dontPattern: Regex = """don't\(\)""".r
    val doPattern: Regex = """do\(\)""".r
    
    var str = input
    var end = false
    while
      !end
    do
      dontPattern.findFirstMatchIn(str) match
        case Some(dontMatch) => {
          doPattern.findFirstMatchIn(str.substring(dontMatch.end)) match
            case Some(doMatch) => {
              str = str.substring(0, dontMatch.start) + str.substring(dontMatch.end + doMatch.end)
            }
            case None => {
              str = str.substring(0, dontMatch.start)
              end = true
            }
        }
        case None => end = true
    
    partOne(str)
