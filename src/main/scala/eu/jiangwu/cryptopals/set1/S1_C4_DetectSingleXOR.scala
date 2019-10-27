package eu.jiangwu.cryptopals
package set1

import eu.jiangwu.cryptopals.set1.S1_C3_SingleByteXOR._

object S1_C4_DetectSingleXOR {
  def run: Unit = {
    val path: String = "set1/4.txt"
    val tops: Array[(Char, String, Double, String)] = detectSingleXOR(path)
    Console.println("Reading list from " + path)
    Console.println("Tops score for each XORed strings:\n" + tops
        .map(x => "\t\t" + x._2 + " -> Score " + x._3 + " with char " + x._1 + ": " + x._4).mkString("\n"))
  }

  /**
    *
    * @param path path of the list on resource folder
    * @return array of (Top char, original string, score with top char, decoded string)
    */
  def detectSingleXOR(path: String): Array[(Char, String, Double, String)] = {
    readListFromResource(path).map{ c =>
      singleByteXORMap(c.toByteArrayFromHexString).map(v => (v._1, c, scoreWords(v._2.toCharString), v._2.toCharString)).maxBy(_._3)
    }.toArray.sortBy(_._3)
  }
}
