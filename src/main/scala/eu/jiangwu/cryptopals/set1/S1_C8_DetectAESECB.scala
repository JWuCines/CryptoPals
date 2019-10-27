package eu.jiangwu.cryptopals
package set1

object S1_C8_DetectAESECB {

  def run: Unit = {
    readListFromResource("set1/8.txt")
      .map(hex => (detectAESECB(hex.toByteArrayFromHexString), hex))
      .filter(_._1)
      .foreach(v => Console.println(v._1 + ": "+ v._2))
  }

  def detectAESECB(s: Array[Byte], blocksize: Int = AES_LENGTH): Boolean =
      s.length % blocksize == 0 && s.grouped(blocksize).map(_.toHexString).toSet.size != (s.length / blocksize)
}
