package lv.rbs.ds.lab03

import play.api.libs.json.{JsValue, Json}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class KMPmatcher(var pattern: String) {
  var comparisons: Int = 0
  var lookupTable = List()
  var result: ArrayBuffer[Int] = ArrayBuffer()
  def getPrefixFun(): ArrayBuffer[Int] = {
    val lookupTable = ArrayBuffer.fill(pattern.length)(-1)
    lookupTable(0) = 0
    var len = 0
    var i = 1
    while (i < pattern.length) {
      if (pattern(i) == pattern(len)) {
        len += 1
        lookupTable(i) = len
        i += 1
      }
      else {
        if (len == 0) {
          lookupTable(i) = 0;
          i = i + 1
        }
        else {
          len = lookupTable(len-1)
        }
      }
    }
    lookupTable
  }

  def findAllIn(t: CharSequence): Iterator[Int] = {
    var result: ArrayBuffer[Int] = ArrayBuffer()
    if (pattern.length > t.length) {
      println("bad input")
    }
    if (pattern == t) {
      println("same strings")
    }
    else {
      val lookupTable = getPrefixFun()
      var i = 0
      var j = 0

      while (i < t.length) {
        if (pattern(j) == t.charAt(i)) {
          i += 1
          j += 1
          comparisons += 1
        }
        if (j == pattern.length) {
          println(s"pattern found at ${i - j}")
          result += (i - j)
          j = lookupTable(j - 1)
        }
        else {
          if (i < t.length && t.charAt(i) != pattern(j)) {
            if (j != 0) {
              j = lookupTable(j - 1)
              comparisons += 1
            }
            else {
              i += 1
            }
          }
        }
      }
    }
    this.comparisons += 3
    println(comparisons)
    result.toIterator
  }

  def toJson(text: CharSequence): String = {
    val jsonAlgorithm: JsValue = Json.toJson("KMP")
    val jsonPattern: JsValue = Json.toJson(pattern)
    val jsonText: JsValue = Json.toJson(text.toString)
    var prefixFunElements = getPrefixFun
    var prefixFunList = new ListBuffer[List[Int]]
    findAllIn(text)

    for (i <- prefixFunElements.indices) {
      prefixFunList.append(List(i, prefixFunElements(i)))
    }
    val jsonPrefixFun: JsValue = Json.toJson(prefixFunList.toList)
    val jsonComparisons: JsValue = Json.toJson(comparisons)

    val jsonMap: Map[String, JsValue] = Map(
      "algorithm" -> jsonAlgorithm,
      "pattern" -> jsonPattern,
      "text" -> jsonText,
      "prefixFun" -> jsonPrefixFun,
      "comparisons" -> jsonComparisons
    )
    val result = Json.stringify(Json.toJson(jsonMap))
    result
  }
}

// this web page helped me to write the code - https://codereview.stackexchange.com/questions/212068/kmp-algorithm-in-scala