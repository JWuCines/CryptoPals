package eu.jiangwu.cryptopals
package set3

import eu.jiangwu.cryptopals.set1.{S1_C1_HexToBase64, S1_C2_FixedXOR}
import eu.jiangwu.cryptopals.set2.S2_C10_CBCMode

object S3_C18_AESCTR {

    def run: Unit = {
      val key: Array[Byte] = "YELLOW SUBMARINE".getBytes
      val s: Array[Byte] = S1_C1_HexToBase64.base64Decode("L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==")
      val d: Array[Byte] = aesCTRDecrypt(s, key)
      assert(aesCTREncrypt(d, key).deep == s.deep)
      Console.println("The decrypted message is: " + d.toCharString)
    }

    def aesCTREncrypt(data: Array[Byte], key: Array[Byte], nonce: Array[Byte] = 0.toByte.multiple(AES_LENGTH/2)): Array[Byte] = {
      data.grouped(AES_LENGTH).zipWithIndex.flatMap { case (block, i) =>
        val bytes = BigInt(i).toByteArray
        val completeNonce = nonce ++ bytes ++ 0.toByte.multiple(AES_LENGTH/2 - bytes.size)
          S1_C2_FixedXOR.xor(
            S2_C10_CBCMode.aesECBEncrypt(completeNonce, key).take(block.size),
            block
          )
      }.toArray
    }

    def aesCTRDecrypt(data: Array[Byte], key: Array[Byte], nonce: Array[Byte] = 0.toByte.multiple(AES_LENGTH/2)): Array[Byte] =
      aesCTREncrypt(data, key, nonce)
}
