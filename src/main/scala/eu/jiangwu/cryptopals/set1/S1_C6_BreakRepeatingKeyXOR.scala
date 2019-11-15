package eu.jiangwu.cryptopals
package set1

object S1_C6_BreakRepeatingKeyXOR {
  def run: Unit = {
    val path: String = "set1/6.txt"
    val message: String = readArrayFromResource(path).mkString
    val (_, output, key) = breakRepeatingKeyXOR(message)
    Console.print(">> Key found:\n\t" + key + "\n>> Text: \n" + output)
  }

  /*
    * The bitwise-xor of two strings will have ones
    * only in places where the two strings have different bits
    * hence, computing the hamming distance of the two strings
    * is equivalent to counting the numbers of one (the "hamming weight") of their XOR sum
    */
  def hamming(s1: String, s2: String): Int = {
    hamming(s1.getBytes, s2.getBytes)
  }

  def hamming(b1: Array[Byte], b2: Array[Byte]): Int = {
    S1_C2_FixedXOR.xor(b1, b2).map(_.toBinaryString.toCharArray.count(_ == '1')).sum
  }

  def breakRepeatingKeyXOR(s: String): (Int, String, String) = {
    val dec64: Array[Byte] = S1_C1_HexToBase64.base64Decode(s)
    val keysize: Int = (2 until 41).map { size =>
        val avg_distance: Double = dec64.grouped(size).take(4).toArray.combinations(2)
                                        .map{ case Array(a1, a2) => hamming(a1, a2) }.sum / 6
        (size, avg_distance / size)
    }.minBy(_._2)._1
    val xored: Array[(Byte, Array[Byte], Double)] = (0 until keysize).map { i =>
      S1_C3_SingleByteXOR.scoredSingleByteXORMap(Range(i, dec64.length, keysize).map(j => dec64(j)).toArray).maxBy(_._3)
    }.toArray
    val key: String = xored.map(_._1).mkString
    val messageParts: Array[Array[Byte]] = xored.map(_._2)
    val message: String = messageParts.maxBy(_.length).indices.flatMap { i =>
      messageParts.flatMap(part => if(part.length >= i+1) Option(part(i)) else None)
    }.toArray.toCharString
    (keysize, message, key)
  }
}