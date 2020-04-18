package lv.rbs.ds.lab03

import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class BMmatcher(pattern: String) {
  var comparisons: Int = 0
  var m = pattern.length

  /**
   * This method should return a list of length m (length of the pattern).
   * Good Suffix function
   */
  def getGoodSuffixFun(): List[Int] = {
    val oneList: ListBuffer[Int] = ListBuffer.fill(m + 1)(0)
    val pi: List[Int] = getPrefixFun(pattern)
    val reversePattern: String = pattern.reverse
    val reversePi: List[Int] = getPrefixFun(reversePattern)

    for (i <- 0 to m) {
      oneList(i) = m - pi(m)
    }

    for (l <- 1 to m) {
      val x = m - reversePi(l)
      oneList(x) = Math.min(oneList(x), l - reversePi(l))
    }
    oneList.toList
  }

  def getBadCharFun(): mutable.HashMap[Char, Int] = {
    val result: mutable.HashMap[Char, Int] = new mutable.HashMap[Char, Int]
    for (i <- 0 until m) {
      result += ((pattern(i), i))
    }
    result
  }

  def findAllIn(text: CharSequence): Iterator[Int] = {
    val n = text.length
    val m = pattern.length
    var s = 0
    val goodSuffix = getGoodSuffixFun()
    val badChar = getBadCharFun()
    val result: ListBuffer[Int] = new ListBuffer[Int]
    while (s <= n - m) {
      var j = m
      while (j > 0 && pattern(j - 1) == text.charAt(s + j - 1)) {
        j = j - 1
        comparisons += 1
      }
      if (j == 0) {
        println("Pattern appears with the offset " + s)
        result += s
        s = s + goodSuffix(0)
        println("start " + (j - 1))
      } else {
        var x = -1
        if(badChar.contains(text.charAt(s + j - 1))){
          x = j - 1 - badChar.apply(text.charAt(s + j - 1))
          println("start " + (j - 1))
          //println(goodSuffix)
          //println("end " + (s))
        }
        s = s + Math.max(goodSuffix(j), x)
      }
    }
    this.comparisons += 3
    println(comparisons)
    result.iterator
  }

  def toJson(text: CharSequence): String = {
    val jsonAlgorithm: JsValue = Json.toJson("BM")
    val jsonPattern: JsValue = Json.toJson(pattern)
    val jsonText: JsValue = Json.toJson(text.toString)
    val goodSuffixFunElements = getGoodSuffixFun()
    val goodSuffixFunList = new ListBuffer[List[Int]]
    findAllIn(text)

    for (i <- goodSuffixFunElements.indices) {
      goodSuffixFunList.append(List(i, goodSuffixFunElements(i)))
    }
    val jsonPrefixFun: JsValue = Json.toJson(goodSuffixFunList.toList)
    val jsonComparisons: JsValue = Json.toJson(comparisons)

    val jsonMap: Map[String, JsValue] = Map(
      "algorithm" -> jsonAlgorithm,
      "pattern" -> jsonPattern,
      "text" -> jsonText,
      "prefixFun" -> jsonPrefixFun,
      "comparisons" -> jsonComparisons
    )
    val result = Json.stringify(Json.toJson(jsonMap))
    println(result)
    result
  }

  def getPrefixFun(pattern: String): List[Int] = {
    val len: Int = pattern.length
    val p: ListBuffer[Int] = ListBuffer.fill(len + 1)(0)
    p(0) = -1
    var border: Int = 0
    for (i <- 2 to len) {
      while (border > 0 && pattern(i-1) != pattern(border)) {
        border = p(border)
      }
      if (pattern(i-1) == pattern(border)) {
        border += 1
      }
      p(i) = border
    }
    p.toList
  }
}