import java.io.File
import scala.collection.mutable

object main {
  def main(args: Array[String]): Unit = {
    val file = new File("./input.txt")
    val lines = scala.io.Source.fromFile(file).getLines.toArray

    val numUnsafe = lines.map(line => {
      val nums = line.split(" ").map(_.toInt)
      nums match {
        case arr if (arr.sorted sameElements arr) || (arr.sorted.reverse sameElements arr) =>
          val diffArray = nums.sliding(2).map { case Array(a, b) => Math.abs(b - a) }.toArray
          diffArray match {
            case diffs if diffs.forall(diff => diff > 0 && diff < 4) => 1
            case _ => 0
          }
        case _ => 0
      }
    }).sum
    
    println(numUnsafe) // Part 1


    val numUnsafeWithOneLevelDiff = lines.map(line => {
      val nums = line.split(" ").map(_.toInt) //original array
      
      
      //get all permuations of the array with one removal
      
      val allNumsToTest = nums.indices.map(i => {
        val arrayBuf = mutable.ArrayBuffer(nums: _*)
        arrayBuf.remove(i)
        arrayBuf.toArray
      }).appended(nums)
      
      
      
      
      
      val checkFunc = (x: Array[Int]) => {
        x match {
          case arr if (arr.sorted sameElements arr) || (arr.sorted.reverse sameElements arr) =>
            val diffArray = x.sliding(2).map { case Array(a, b) => Math.abs(b - a) }.toArray
            diffArray match {
              case diffs if diffs.forall(diff => diff > 0 && diff < 4) => true
              case _ => false
            }
          case _ => false
        }
      }
      
      allNumsToTest.map(checkFunc).filter(_ == true) match {
        case array if array.nonEmpty => 1
        case _ => 0
      }
     
      
      
    }).sum
    
    
    println(numUnsafeWithOneLevelDiff) // Part 2
    
  }
}
