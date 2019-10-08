package eu.jiangwu.cryptopals
package setone

object S1_C8_DetectAESECB {

  def run(): Unit = {
    val hexList: List[String] = readListFromResource("setone/8.txt")

  }

  def detectAESECB(hex: String): Option[String] = {
    val deHex: Array[Byte] = hex.toByteArrayFromHexString
    None
  }
}
