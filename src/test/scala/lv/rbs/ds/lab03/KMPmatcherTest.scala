package lv.rbs.ds.lab03

import org.scalatest._
import net.liftweb.json._

class KMPmatcherTest extends FlatSpec with Matchers {

  // **************************************************************************
  // You can delete this. It is just to verify that the test suite has correct import statements
  // **************************************************************************
  "A KMP matcher" should "satisfy a trivial test" in {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    1 should be(1)
  }


  // **************************************************************************
  // Check if the search algorithm actually searches substrings correctly
  // **************************************************************************
  ignore should "return 3 items, if they exist" in {
    val matcher = new KMPmatcher("ab")
    val result = matcher.findAllIn("stab, about, above").toList
    result should be(List(2, 6, 13))
  }

  ignore should "return empty findAllIn iterator, if none exist" in {
    val matcher = new KMPmatcher("ab")
    val result = matcher.findAllIn("xxxx, yyyyy, zzzzz").toList
    result should be(List())
  }

  // **************************************************************************
  // Check if the pattern preprocessing functions are correct
  // **************************************************************************
  ignore should "return prefixFun" in {
    val expected = List(-1,0,0,0,0,1,2,0)
    val myPattern = "ABCDABD"
    val matcher = new KMPmatcher(myPattern)
    matcher.getPrefixFun() should be(expected)
  }


  // **************************************************************************
  // Testing the returned JSON as a string.
  // **************************************************************************
  ignore should "return JSON string with the right algorithm type" in {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    result should include("KMP")
  }


  // **************************************************************************
  // Testing the returned JSON as a parsed data structure.
  // **************************************************************************
  ignore should "return 3 correct string fields" in {
    val myPattern = "ABCDABD"
    val matcher = new KMPmatcher(myPattern)
    val myText = "ABC ABCDAB ABCDABCDABDE"
    val result = matcher.toJson(myText)
    val json = parse(result)
    implicit val formats = DefaultFormats
    val aa = (json \ "algorithm").extract[String]
    aa should be("KMP")
    val pp = (json \ "pattern").extract[String]
    pp should be(myPattern)
    val tt = (json \ "text").extract[String]
    tt should be(myText)
  }

  ignore should "count character comparisons correctly" in {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val comparisons = (json \ "comparisons").extract[Int]
    comparisons should be(27)
  }

  ignore should "return correct steps 0, 1 and 6" in {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val resultSteps = (json \ "steps").extract[List[Map[String, String]]]
    resultSteps(0)("offset") should be("0")
    resultSteps(0)("start") should be("0")
    resultSteps(0)("end") should be("3")
    resultSteps(0).keySet.contains("match") should be(false)

    resultSteps(1)("offset") should be("3")
    resultSteps(1)("start") should be("0")
    resultSteps(1)("end") should be("0")
    resultSteps(1).keySet.contains("match") should be(false)

    resultSteps(6)("offset") should be("15")
    resultSteps(6)("start") should be("2")
    resultSteps(6)("end") should be("6")
    resultSteps(6)("match") should be("true")
  }
}
