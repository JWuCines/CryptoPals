package eu.jiangwu.cryptopals
package settwo

import eu.jiangwu.cryptopals.setone.{S1_C1_HexToBase64, S1_C2_FixedXOR, S1_C7_AESECBDecrypt}

object S2_C10_CBCMode {
    def run {
        val key: String = "YELLOW SUBMARINE"
        assert(key.length == 16)
        val ivByte: Byte = 0
        val iv: Array[Byte] = ivByte.multiple(key.length)
        Console.println("Key used " + key + " size: " + key.length)
        Console.println("Initial IV (hex): " + iv.toHexString)
        val dec64: Array[Byte] = S1_C1_HexToBase64.base64Decode(readListFromResource("settwo/10.txt").mkString)
        Console.println("Base64 decode message length: " + dec64.length)
        val dec: Array[Byte] = aes_cbc_decrypt(dec64, key.getBytes, iv)
        val unpad_dec: Array[Byte] = S2_C9_PCKS7Padding.remove_padding(dec)
        Console.println("Last 10 character (no unpad): " + dec.takeRight(10).toHexString)
        Console.println("Last 10 character (unpad): " + unpad_dec.takeRight(10).toHexString)
        Console.println("Unpadded decrypted text:\n" + unpad_dec.toCharString)
    }

    def aes_cbc_decrypt(data: Array[Byte], key: Array[Byte], iv: Array[Byte]): Array[Byte] = {
        var currentIV: Array[Byte] = iv
        data.grouped(key.length).zipWithIndex.flatMap { case(block, i) =>
          val dec: Array[Byte] = S1_C7_AESECBDecrypt.aes_ecb_decrypt(block, key)
          val plain: Array[Byte] = S1_C2_FixedXOR.xor(currentIV, dec)
          currentIV = block
          plain
        }.toArray
    }
}
