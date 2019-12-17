package eu.jiangwu.cryptopals
package set2

import eu.jiangwu.cryptopals.set1.{S1_C1_HexToBase64, S1_C7_AESECBDecrypt, S1_C8_DetectAESECB}

import scala.collection.mutable.ArrayBuffer

class ECBOracle {
    val key = generateRandomArrayByte(AES_LENGTH)
    val secret: Array[Byte] = S1_C1_HexToBase64.base64Decode("Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg" +
      "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq" +
      "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg" +
      "YnkK")
    def getData(data: Array[Byte]): Array[Byte] = data ++ secret
    def encrypt(data: Array[Byte] = Array.empty[Byte]): Array[Byte] = S2_C10_CBCMode.aesECBPKCS5Encrypt(getData(data), key)
}

object S2_C12_ByteAtATimeECBDecrypt {

  def run: Unit = {
    val oracle = new ECBOracle
    val blockSize = findBlockLength(oracle)
    assert(blockSize == AES_LENGTH)
    Console.println("Block size found: " + blockSize)
    assert(S1_C8_DetectAESECB.detectAESECB(oracle.encrypt('A'.toByte.multiple(100))))
    Console.println("Detected AES ECB correctly")
    assert(S2_C9_PCKS7Padding.removePadding(byteAtATimeECBDecryption(oracle)).diff(oracle.secret).isEmpty)
    Console.println("Secret decrypted: \n" + oracle.secret.toCharString)
  }

  def findBlockLength(oracle: ECBOracle): Int = {
    def calcLengthMultipleA(i: Int): Int = oracle.encrypt('A'.toByte.multiple(i)).length
    val inLen = calcLengthMultipleA(0)
    (1 to 256).takeWhile(i => calcLengthMultipleA(i) == inLen)
              .lastOption
              .map(i => calcLengthMultipleA(i + 1) - inLen)
              .get
  }

  def getNextByte(current: Array[Byte], oracle: ECBOracle): Array[Byte] = {
    val blockSize: Int = findBlockLength(oracle)
    val blockNumber: Int = current.length / blockSize
    val prefix = 'A'.toByte.multiple(math.abs(blockSize*(blockNumber+1) - 1 - current.length) % blockSize)
    val targetBlock = oracle.encrypt(prefix).grouped(blockSize).toArray.apply(blockNumber)
    (0 to 255).foreach { c =>
      val fakeBlock = oracle.encrypt(prefix ++ current ++ Array[Byte](c.toByte)).grouped(blockSize).toArray.apply(blockNumber)
      if (fakeBlock.diff(targetBlock).isEmpty)  {
        return Array[Byte](c.toByte)
      }
    }
    Array.empty[Byte]
  }

  def byteAtATimeECBDecryption(oracle: ECBOracle): Array[Byte] = {
    val buffer = new ArrayBuffer[Byte]()
    oracle.encrypt().foreach(_ => buffer ++= getNextByte(buffer.toArray, oracle))
    buffer.toArray
  }
}
