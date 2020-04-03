/*package lv.rbs.ds.lab03

import net.liftweb.json._
import org.scalatest._

class BMmatcherTest extends FlatSpec with Matchers {

  // **************************************************************************
  // You can delete this. It is just to verify that the test suite has correct import statements
  // **************************************************************************
  "A BM matcher" should "satisfy a trivial test" in {
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    1 should be(1)
  }

  // **************************************************************************
  // Check if the search algorithm actually searches substrings correctly
  // **************************************************************************
  ignore should "return 3 items in findAllIn iterator, if they exist" in {
    val matcher = new BMmatcher("ab")
    val result = matcher.findAllIn("stab, about, above").toList
    result should be(List(2, 6, 13))
  }

  ignore should "return empty findAllIn iterator, if none exist" in {
    val matcher = new BMmatcher("ab")
    val result = matcher.findAllIn("xxxx, yyyyy, zzzzz").toList
    result should be(List())
  }

  ignore should "return 1 item in findAllIn iterator" in {
    val matcher = new BMmatcher("ABBABAB")
    val result = matcher.findAllIn("ABAABABBBBABBBAAABBABAB").toList
    result should be(List(16))
  }


  // **************************************************************************
  // Check if the pattern preprocessing functions are correct
  // **************************************************************************
  ignore should "return goodSuffixFun" in {
    val expected = List(-7,-6,-5,-4,-3,2,5)
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    matcher.getGoodSuffixFun() should be(expected)
  }

  ignore should "return badCharacterFun" in {
    val expected:List[(Char,Int)] = List(('A',4),('B',5),('C',2),('D',6))
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    matcher.getBadCharFun() should be(expected)
  }


  // **************************************************************************
  // Testing the returned JSON as a string.
  // **************************************************************************
  ignore should "return JSON string with the right algorithm type" in {
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    result should include("BM")
  }

  // **************************************************************************
  // Testing the returned JSON as a parsed data structure.
  // **************************************************************************
  ignore should "return 3 correct string fields" in {
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    val myText = "ABC ABCDAB ABCDABCDABDE"
    val result = matcher.toJson(myText)
    val json = parse(result)
    implicit val formats = DefaultFormats
    val aa = (json \ "algorithm").extract[String]
    aa should be("BM")
    val pp = (json \ "pattern").extract[String]
    pp should be(myPattern)
    val tt = (json \ "text").extract[String]
    tt should be(myText)
  }

  ignore should "return correct steps 0, 1 and 3" in {
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val resultSteps = (json \ "steps").extract[List[Map[String, String]]]
    resultSteps(0)("offset") should be("0")
    resultSteps(0)("start") should be("6")
    resultSteps(0)("end") should be("6")
    resultSteps(0).keySet.contains("match") should be(false)

    resultSteps(1)("offset") should be("4")
    resultSteps(1)("start") should be("6")
    resultSteps(1)("end") should be("6")
    resultSteps(1).keySet.contains("match") should be(false)

    resultSteps(3)("offset") should be("15")
    resultSteps(3)("start") should be("6")
    resultSteps(3)("end") should be("0")
    resultSteps(3)("match") should be("true")
  }



}
*/