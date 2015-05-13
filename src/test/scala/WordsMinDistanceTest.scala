import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}

/**
 * Created by anupamam on 5/10/15.
 */
class WordsMinDistanceTest extends FlatSpec
  with Matchers
  with BeforeAndAfterEach
  with BeforeAndAfterAll {
    val filePath = "src/test/resources/MK-Bio"


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








}

