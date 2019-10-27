package eu.jiangwu.cryptopals
package set2

import eu.jiangwu.cryptopals.set1.{S1_C1_HexToBase64, S1_C2_FixedXOR, S1_C7_AESECBDecrypt}
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object S2_C10_CBCMode {
    def run {
        val key: String = "YELLOW SUBMARINE"
        assert(key.length == AES_LENGTH)
        val ivByte: Byte = 0
        val iv: Array[Byte] = ivByte.multiple(key.length)
        Console.println("Key used " + key + " size: " + key.length)
        Console.println("Initial IV (hex): " + iv.toHexString)
        val dec64: Array[Byte] = S1_C1_HexToBase64.base64Decode(readListFromResource("set2/10.txt").mkString)
        Console.println("Base64 decode message length: " + dec64.length)
        val dec: Array[Byte] = aesCBCDecrypt(dec64, key.getBytes, iv)
        val unpadDec: Array[Byte] = S2_C9_PCKS7Padding.removePadding(dec)
        Console.println("Last 10 character (no unpad): " + dec.takeRight(10).toHexString)
        Console.println("Last 10 character (unpad): " + unpadDec.takeRight(10).toHexString)
        Console.println("Unpadded decrypted text:\n" + unpadDec.toCharString)
        val enc: Array[Byte] = aesCBCEncrypt(dec, key.getBytes, iv)
        Console.println("Encoded back are equal: " + (enc.deep == dec64.deep))
    }

    def aesECBEncrypt(data: Array[Byte], key: Array[Byte], mode: String = "AES/ECB/NoPadding"): Array[Byte] = {
      encrypt(data, key, "AES", mode)
    }

    def aesECBPKCS5Encrypt(data: Array[Byte], key: Array[Byte]): Array[Byte] = {
      aesECBEncrypt(S2_C9_PCKS7Padding.padding(data, key.length), key, "AES/ECB/PKCS5Padding")
    }

    def encrypt(data: Array[Byte], key: Array[Byte], algo: String, mode: String): Array[Byte] = {
      val cipher: Cipher = Cipher.getInstance(mode)
      cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, algo))
      cipher.doFinal(data)
    }

    def aesCBCDecrypt(data: Array[Byte], key: Array[Byte], iv: Array[Byte]): Array[Byte] = {
        var currentIV: Array[Byte] = iv
      S2_C9_PCKS7Padding.removePadding(
        data.grouped(key.length).flatMap { block =>
          val dec: Array[Byte] = S1_C7_AESECBDecrypt.aesECBDecrypt(S2_C9_PCKS7Padding.padding(block, key.length), key)
          val plain: Array[Byte] = S1_C2_FixedXOR.xor(currentIV, dec)
          currentIV = block
          plain
        }.toArray
      )
    }

    def aesCBCEncrypt(data: Array[Byte], key: Array[Byte], iv: Array[Byte]): Array[Byte] = {
      var currentIV: Array[Byte] = iv
      data.grouped(key.length).flatMap { block =>
        val padBlock = S2_C9_PCKS7Padding.padding(block, key.length)
        val xoredBlock: Array[Byte] = S1_C2_FixedXOR.xor(padBlock, currentIV)
        val enc: Array[Byte] = aesECBEncrypt(xoredBlock, key)
        currentIV = enc
        enc
      }.toArray
    }
}
