package eu.jiangwu.cryptopals
package setone

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object S1_C7_AESECBDecrypt {

  def run(key: String): Unit = {
    val algo: String = "AES"
    val mode: String = "AES/ECB/NoPadding"
    Console.println("Algorithm used: " + algo)
    Console.println("Algorithm mode used: " + mode)
    Console.println("Key used: " + key)
    val s: String = readListFromResource("setone/7.txt").mkString
    Console.println(">> Decoded text: \n" + decrypt(s, key, algo, mode))
  }

  def decrypt(base64: String, key: String, algo: String, mode: String): String = {
    val s: Array[Byte] = S1_C1_HexToBase64.base64Decode(base64)
    val cipher: Cipher = Cipher.getInstance(mode)
    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key.getBytes, algo))
    cipher.doFinal(s).toCharString.stripMargin.trim
  }
}
