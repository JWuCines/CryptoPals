package eu.jiangwu.cryptopals
package set3

import eu.jiangwu.cryptopals.set1.{S1_C1_HexToBase64, S1_C2_FixedXOR, S1_C3_SingleByteXOR}

object S3_C19_BreakFixedNonceCTRSubstitution {

    def run: Unit = {
      val key: Array[Byte] = generateRandomArrayByte()
      val encData: Array[Array[Byte]] = readArrayFromResource("set3/19.txt")
                                          .map(s => S3_C18_AESCTR.aesCTREncrypt(S1_C1_HexToBase64.base64Decode(s), key))
      Console.println("Generated key: " + key.toCharString)
      Console.println("Original strings: \n" + encData.map(s => S3_C18_AESCTR.aesCTRDecrypt(s, key).toCharString).mkString("\n"))
      val decData: Array[Array[Byte]] = forceCTRSameNonce(encData)
      val encDataAfter: Array[Array[Byte]] = decData.map(d => S3_C18_AESCTR.aesCTREncrypt(d, key))
      //assert(!encData.zip(encDataAfter).exists(x => x._1.deep != x._2.deep))
      Console.println("Decrypted strings: \n" + decData.map(_.toCharString).mkString("\n"))
    }

    def getKeystream(data: Array[Array[Byte]]): Array[Byte] = {
      val opt = (0 until data.map(_.length).max)
        .map(i => data.flatMap(_.lift(i)))
        .map(d => S1_C3_SingleByteXOR.scoredSingleByteXORMap(d, 0 until 128))
      //opt.zipWithIndex
      //  .foreach(v => Console.println("Index " + v._2 + ": " + v._1.map(t => t._1.toChar + " " + t._3 * t._4 + " - " + t._2.toCharString).mkString("\n")))
      opt.map(_.maxBy(v => v._3)._1).toArray
    }
    def forceCTRSameNonce(data: Array[Array[Byte]]): Array[Array[Byte]] = {
      val keyStream = getKeystream(data)
      Console.println("KeyStream: " + keyStream.toCharString)
      data.map(d => S1_C2_FixedXOR.xor(d, keyStream.take(d.length)))
    }
}
