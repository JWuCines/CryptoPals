package eu.jiangwu.cryptopals
package set1

object S1_C3_SingleByteXOR {
  def run: Unit = {
    val in1: String = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    Console.println("Input XORed String: " + in1)
    Console.println("Reversed XOR Strings:\n" + scoredSingleByteXORMap(in1.toByteArrayFromHexString)
        .map(x => "\t\t" + x._1.toChar + " -> Score = " + x._3+ ": "+ x._2.toCharString).mkString("\n"))
  }

  def scoredSingleByteXORMap(in1: Array[Byte], r: Range = 32 to 127): Array[(Byte, Array[Byte], Double)] = {
    singleByteXORMap(in1, r).map(s => (s._1, s._2, scoreWords(s._2.toCharString))).sortBy(_._3)
  }

  def singleByteXORMap(in1: Array[Byte], r: Range = 32 to 127): Array[(Byte, Array[Byte])] = {
    r.map{ c =>
      (c.toByte, singleByteXOR(in1, c.toByte))
    }.toArray
  }

  def singleByteXOR(in1: Array[Byte], b: Byte): Array[Byte] = S1_C2_FixedXOR.xor(in1, b.multiple(in1.length))
}
