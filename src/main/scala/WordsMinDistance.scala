import java.io.FileNotFoundException
import scala.io.Source

/**
 * Created by anupamam on 5/10/15.
 * Algorithm to compute the minimum window
 * in a text file containing all the searched words
 */
object WordsMinDistance {

  /**
   *
   * @param num list of indices of all words
   * @return min window length containing all words
   */
  def minDistance(num: List[List[Int]]): Int = {
    val heads = num.filter(list => !list.isEmpty).map(list => list.head)
    heads.size match {
      case 0 => -1
      case _ => heads.reduceLeft(_ max _) - heads.reduceLeft(_ min _)
    }
  }


  /**
   * This function moves the pointer of the list containing the smallest indices
   * @param indexes list of indices of all words in the text
   * @return updated list of indices
   */

  def updateIndexes(indexes: List[List[Int]]): List[List[Int]] = {
    val minHead = indexes.filter(list => list.nonEmpty).
      map(f => f.head).reduceLeft(_ min _)
    val newIndexes = indexes.map(list =>
      if (list.head == minHead)
        list.drop(1)
      else
        list
    )
    newIndexes
  }

  /**
   * This function is called recursively for the list of indices
   * for all the search words
   *
   * @param indexes list of index list of individual words
   * @return minium window length containing all search words
   */
  def findDistance(indexes: List[List[Int]], minWindow: Int): Int = {
    indexes match {
      case Nil =>
        return minWindow
      case _ =>
        if (!indexes.map(f => f.size).contains(0)) {
          val minWindowLength = minDistance(indexes)
          if (minWindowLength < minWindow)
            return findDistance(updateIndexes(indexes), minWindowLength)
          else
            return findDistance(updateIndexes(indexes), minWindow)
        } else {
          return minWindow
        }
    }
  }

  /**
   * utility function to display error message
   * @param message
   */
  def usage(message: String) = {
    println("Error:" + message)
  }

  /**
   * Starting point for the algorithm.
   * This function validates the input
   * Parses the input text file into list of indices of the search words
   * @param filePath
   * @param searchWords
   * @return returns
   *         -1 invalid input
   *         0 single word search
   *         >0 min Window Length
   */
  def apply(filePath: String, searchWords: List[String]): Int = {

    val minDistance: Int =

      if (filePath == null) {
        usage("Invalid file")
        return -1

      } else if (searchWords.isEmpty || searchWords.size < 2) {
        usage("Insufficient query words")
        return 0

      } else {
        val words = try {
          Source.fromFile(filePath).mkString.split("""\W+""")
        } catch {
          case ex: FileNotFoundException => {
            usage("Missing File Exception")
            return -1
          }
        }
        val pairs = words.toList.zip(words.indices).groupBy {
          case (k, v) => k
        }.filter(k => searchWords.contains(k._1))
          .map {
          case (k, v) =>
            (v.map(_._2))
        }.toList
        findDistance(pairs, Int.MaxValue)
      }

    if (minDistance == Int.MaxValue)
      -1
    else
      minDistance
  }

  def main (args: Array[String]) {
    if(args.length<2)
      usage("Invalid Input")
    else{
      val searchWords = args(1).toString.split("""\W+""").toList
      println("Window length:"+ WordsMinDistance(args(0), searchWords))

    }
  }

}
