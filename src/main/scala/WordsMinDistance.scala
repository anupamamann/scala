import java.io.FileNotFoundException
import scala.io.Source

/**
 * Created by anupamam on 5/10/15.
 * Algorithm to compute the minimum window
 * in a text file containing all the searched words
 */
object WordsMinDistance {

  /**
   * TODO: return the number of words between the searched words
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

        /**
         * case: search two occurrence of the same word
         * return error if the word occurs only once
         * else return the distance between first two occurrences
         */

        if (indexes.size == 1) {
          if (indexes.head.size < 2)
            minWindow
          else
            indexes.flatMap(l => l.take(2)).reduceLeft { (a, b) => (b - a) }
        } else if (!indexes.map(f => f.size).contains(0)) {
          val minWindowLength = minDistance(indexes)
          if (minWindowLength < minWindow)
            findDistance(updateIndexes(indexes), minWindowLength)
          else
            findDistance(updateIndexes(indexes), minWindow)
        } else {
          minWindow
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
        return -1

      } else {
        val words = try {
          Source.fromFile(filePath).mkString.split( """\W+""")
        } catch {
          case ex: FileNotFoundException => {
            usage("Missing File Exception")
            return -1
          }
        }

        /*
          generate list of indices of the search words
          pairs => List[List[Int]]
         */
        val pairs = words.toList.zip(words.indices).groupBy {
          case (k, v) => k
        }.filter(k => searchWords.contains(k._1))
          .map {
          case (word, indexList) =>
            (indexList.map(_._2))
        }.toList

        /**
         * case 1: Not all words were found in the text
         */
        if (pairs.size != searchWords.distinct.size)
          return -1

        /**
         * case 1: Searching for two instances of same word
         * case 2: Searching for different words
         */

        findDistance(pairs, Int.MaxValue)
      }

    /**
     * case 1: findDistance returns MaxValue
     * if the searched words were not found in the text
     *
     * case2:
     * consecutive occurrence of words,
     * the distance is 0 as there is no word in between the searched word
     *
     *  case3:
     * for all other minDistance represents
     * the number of words in between searched words
     */
    if (minDistance == Int.MaxValue)
      -1
    else if (minDistance == searchWords.distinct.size - 1)
      0
    else
      minDistance - 1

  }

  def main(args: Array[String]) {
    if (args.length < 2)
      usage("Invalid Input")
    else {
      val searchWords = args(1).toString.split( """\W+""").toList
      println("Window length:" + WordsMinDistance(args(0), searchWords))

    }
  }

}
