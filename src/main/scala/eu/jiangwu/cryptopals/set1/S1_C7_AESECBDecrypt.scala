package eu.jiangwu.cryptopals
package set1

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object S1_C7_AESECBDecrypt {

  def run(key: String): Unit = {
    val algo: String = "AES"
    val mode: String = "AES/ECB/NoPadding"
    Console.println("Algorithm used: " + algo)
    Console.println("Algorithm mode used: " + mode)
    Console.println("Key used: " + key)
    val s: String = readArrayFromResource("set1/7.txt").mkString
    val data: Array[Byte] = S1_C1_HexToBase64.base64Decode(s)
    val keyData: Array[Byte] = key.getBytes
    val decData: Array[Byte] = decrypt(data, keyData, algo, mode)
    assert(decData.deep == aesECBDecrypt(data, keyData).deep)
    Console.println(">> Decoded text: \n" + decData.toCharString.stripMargin.trim)
  }

  def aesECBDecrypt(data: Array[Byte], key: Array[Byte], mode: String = "AES/ECB/NoPadding"): Array[Byte] = {
    decrypt(data, key, "AES", mode)
  }

  def aesECBPKCS5Decrypt(data: Array[Byte], key: Array[Byte]): Array[Byte] = {
    aesECBDecrypt(data, key, "AES/ECB/PKCS5Padding")
  }

  def decrypt(data: Array[Byte], key: Array[Byte], algo: String, mode: String): Array[Byte] = {
    val cipher: Cipher = Cipher.getInstance(mode)
    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, algo))
    cipher.doFinal(data)
  }
}
