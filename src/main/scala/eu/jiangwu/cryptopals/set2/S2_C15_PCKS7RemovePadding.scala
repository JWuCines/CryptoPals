package eu.jiangwu.cryptopals
package set2
import S2_C9_PCKS7Padding._
object S2_C15_PCKS7RemovePadding {

    def run = {
      val test1 = "ICE ICE BABY\04\04\04\04"
      val test1Valid = isValidPadding(test1.getBytes)
      val test1Ret = removePadding(test1.getBytes).toCharString
      val ret1 = "ICE ICE BABY"
      assert(test1Valid)
      assert(test1Ret.contentEquals(ret1))
      Console.println(test1 + " padding is valid: " + test1Valid + " - Result: " + test1Ret)

      val test2 = "ICE ICE BABY\05\05\05\05"
      val test2Valid = isValidPadding(test2.getBytes)
      assert(!test2Valid)
      Console.println(test2 + " padding is valid: " + test2Valid)

      val test3 = "ICE ICE BABY\01\02\03\04"
      val test3Valid = isValidPadding(test3.getBytes)
      assert(!test3Valid)
      Console.println(test3 + " padding is valid: " + test3Valid)
    }

    def isValidPadding(data: Array[Byte]): Boolean = data.endsWith(data.last.multiple(data.last.toInt))
}
