import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by anupamam on 5/10/15.
 * Reviewed by maasg on 05/18/15.
 */
class WordsMinDistanceTest extends FlatSpec with Matchers {
    val filePath = "src/test/resources/MK-Bio"
    val abcPath = "src/test/resources/abc.txt"
    val baabPath = "src/test/resources/baab.txt"


  "min Distance" should "not run with null file" in {
    val searchWords = List("born", "family")
    assert(WordsMinDistance(null, searchWords) == -1)

  }

  "min Distance" should "not run with wrong file path" in {
    val searchWords = List("born", "family")
    assert(WordsMinDistance("hello", searchWords) == -1)

  }

  "min Distance" should "not run with insufficient search words" in {
    val searchWords = List("born")
    assert(WordsMinDistance(filePath, searchWords) == 0)

  }

  "min Distance" should "be 11" in {
    val searchWords = List("born", "family")
    assert(WordsMinDistance(filePath, searchWords) == 11)
  }

  "min Distance" should "be 278" in {
    val searchWords = List("born", "family", "married")
    assert(WordsMinDistance(filePath, searchWords) == 278)

  }

  "min Distance" should "be -1, words do not exist in text" in {
    val searchWords = List("bleh", "rule", "hellooo")
    assert(WordsMinDistance(filePath, searchWords) == -1)

  }

  "min Distance" should "be 0 in three consecutive words" in {
    WordsMinDistance(abcPath, List("a","b","c")) should be (0)
  }

  "min Distance" should "be 0 in two consecutive equal words" in {
    WordsMinDistance(abcPath, List("a","a")) should be (0)
  }

  "min Distance" should "be 0 in two consecutive words" in {
    WordsMinDistance(abcPath, List("a","b")) should be (0)
  }

  "min Distance" should "be 1 in two words with one in between" in {
    WordsMinDistance(abcPath, List("a","c")) should be (1)
  }

  "min Distance" should "be 0 in two consecutive words (BC)" in {
    WordsMinDistance(abcPath, List("b","c")) should be (0)
  }

  "min Distance" should "be -1 when only one word is found" in {
    WordsMinDistance(abcPath, List("b","b")) should be (0)
  }

  "min Distance" should "be 2 when the same word is present with 2 words in between" in {
    WordsMinDistance(baabPath, List("b","b")) should be (2)
  }

  "min Distance" should "be -1 when the first word is not in the text" in {
    WordsMinDistance(baabPath, List("u","b")) should be (-1)
  }

  "min Distance" should "be -1 when the second word is not in the text" in {
    WordsMinDistance(baabPath, List("b","u")) should be (-1)
  }

  "min Distance" should "be 6 when the words are spread on the text with 6 words in between" in {
    WordsMinDistance(baabPath, List("a","c","e")) should be (6)
  }
}

