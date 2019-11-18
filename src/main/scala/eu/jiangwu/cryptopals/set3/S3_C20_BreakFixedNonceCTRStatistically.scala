package eu.jiangwu.cryptopals
package set3

import eu.jiangwu.cryptopals.set1.S1_C1_HexToBase64

object S3_C20_BreakFixedNonceCTRStatistically {

    def run: Unit = {
      val key: Array[Byte] = generateRandomArrayByte()
      val encData: Array[Array[Byte]] = readArrayFromResource("set3/20.txt")
                                          .map(s => S3_C18_AESCTR.aesCTREncrypt(S1_C1_HexToBase64.base64Decode(s), key))
      Console.println("Generated key: " + key.toCharString)
      Console.println("Original strings: \n" + encData.map(s => S3_C18_AESCTR.aesCTRDecrypt(s, key).toCharString).mkString("\n"))
      val decData: Array[Array[Byte]] = S3_C19_BreakFixedNonceCTRSubstitution.forceCTRSameNonce(encData)
      val encDataAfter: Array[Array[Byte]] = decData.map(d => S3_C18_AESCTR.aesCTREncrypt(d, key))
      //assert(!encData.zip(encDataAfter).exists(x => x._1.deep != x._2.deep))
      Console.println("Decrypted strings: \n" + decData.map(_.toCharString).mkString("\n"))
    }
}
