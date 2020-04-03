/*package lv.rbs.ds.lab03

import org.scalatest._

class KMPpublicTest  extends FunSuite {
  test("Prefix function for abab") {
    val matcher = new KMPmatcherSecret("abab")
    assert(matcher.getPrefixFun().toList === List(-1,0,0,1,2))
  }

  test("Prefix function for aabaab") {
    val matcher = new KMPmatcherSecret("aabaab")
    assert(matcher.getPrefixFun().toList === List(-1,0,1,0,1,2,0))
  }


}
*/