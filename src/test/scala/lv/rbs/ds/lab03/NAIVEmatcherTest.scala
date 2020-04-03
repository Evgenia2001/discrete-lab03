/*package lv.rbs.ds.lab03

import org.scalatest._
import net.liftweb.json._
import scala.io.Source

class NAIVEmatcherTest extends FlatSpec with Matchers {

  // **************************************************************************
  // Check if the search algorithm actually searches substrings correctly
  // **************************************************************************
  "A naive matcher" should "return 3 items, if they exist" in {
    val matcher = new NAIVEmatcher("ab")
    val result = matcher.findAllIn("stab, about, above").toList
    result should be(List(2, 6, 13))
  }

  it should "return empty findAllIn iterator, if none exist" in {
    val matcher = new NAIVEmatcher("ab")
    val result = matcher.findAllIn("xxxx, yyyyy, zzzzz").toList
    result should be(List())
  }

  // **************************************************************************
  // Testing the returned JSON as a string.
  // **************************************************************************
  // Writing JSON unittests for JSON as a string is possible.
  // But these tests are not fine-grained; can check substrings or the approximate total length.
  // Other JSON things depend on the formatting.
  "A naive matcher" should "return JSON with the right algorithm type" in {
    val matcher = new NAIVEmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    result should include("Naive")
  }

  it should "return sufficiently long JSON" in {
    val matcher = new NAIVEmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    //reflect.io.File("src/test/resources/sample01-naive.json").writeAll(result)
    result.length should be > 1000
  }

  // WARNING: Such tests are fragile! Try to avoid them.
  // If this test fails on your environment, please comment it out.
  // It's correctness may depend on Windows/Linux style line endings; and also - how you generate your JSON.
  // There are more robust ways to test, if the structure of JSON is correct.
  it should "return exact pretty-printed JSON text" in {
    val expected = Source.fromFile("src/test/resources/sample01-naive.json").getLines.mkString("\n")
    val matcher = new NAIVEmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    result should be(expected)
  }



    // **************************************************************************
  // Testing the returned JSON as a parsed data structure
  // **************************************************************************
  "A naive matcher" should "return 3 correct string fields" in {
    val myPattern = "ABCDABD"
    val matcher = new NAIVEmatcher(myPattern)
    val myText = "ABC ABCDAB ABCDABCDABDE"
    val result:String = matcher.toJson(myText)
    val json = parse(result)
    implicit val formats = DefaultFormats
    val aa = (json \ "algorithm").extract[String]
    aa should be("Naive")
    val pp = (json \ "pattern").extract[String]
    pp should be(myPattern)
    val tt = (json \ "text").extract[String]
    tt should be(myText)
  }

  it should "count character comparisons correctly" in {
    val matcher = new NAIVEmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val comparisons = (json \ "comparisons").extract[Int]
    comparisons should be(40)
  }

  it should "return correct steps #0 and #15" in {
    val matcher = new NAIVEmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val resultSteps = (json \ "steps").extract[List[Map[String,String]]]
    resultSteps(0)("offset") should be ("0")
    resultSteps(0)("start") should be ("0")
    resultSteps(0)("end") should be ("3")
    resultSteps(0).keySet.contains("match") should be(false)

    resultSteps(15)("offset") should be ("15")
    resultSteps(15)("start") should be ("0")
    resultSteps(15)("end") should be ("6")
    resultSteps(15)("match") should be ("true")
  }


  // **************************************************************************
  // These tests use a homegrown class SearchTranscript (not mandatory in your lab!)
  // This class was handy for the NAIVEmatcher as it encapsulates all information
  // related to the JSON generation.
  // **************************************************************************
  "A naive matcher" should "return the right number of steps" in {
    val matcher = new NAIVEmatcher("ABCDABD")
    val result = matcher.findAllSearchSteps("ABC ABCDAB ABCDABCDABDE")
    result.length should be(17)
  }

  it should "return all the right steps" in {
    val expected = List(
      (0, 0, 3, 0), (1, 0, 0, 0), (2, 0, 0, 0), (3, 0, 0, 0), (4, 0, 6, 0),
      (5, 0, 0, 0), (6, 0, 0, 0), (7, 0, 0, 0), (8, 0, 2, 0), (9, 0, 0, 0),
      (10, 0, 0, 0), (11, 0, 6, 0), (12, 0, 0, 0), (13, 0, 0, 0), (14, 0, 0, 0),
      (15, 0, 6, 1), (16, 0, 0, 0)
    )
    val matcher = new NAIVEmatcher("ABCDABD")
    val result = matcher.findAllSearchSteps("ABC ABCDAB ABCDABCDABDE")
    for (i <- 0 until result.length) {
      result(i) should be(expected(i))
    }
  }

}
 */