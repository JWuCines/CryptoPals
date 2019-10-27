package eu.jiangwu.cryptopals
package settwo

import eu.jiangwu.cryptopals.setone.{S1_C1_HexToBase64, S1_C7_AESECBDecrypt, S1_C8_DetectAESECB}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ECBOracleHarder extends ECBOracle {
    val prefix = generateRandomArrayByte(Random.nextInt(10000) % AES_LENGTH)
    override def getData(data: Array[Byte]): Array[Byte] = prefix ++ data ++ secret
}

object S2_C14_ByteAtATimeECBDecryptHarder {

  def run: Unit = {
    val oracle = new ECBOracleHarder
    val blockSize = S2_C12_ByteAtATimeECBDecrypt.findBlockLength(oracle)
    assert(blockSize == AES_LENGTH)
    Console.println("Block size found: " + blockSize)
    assert(S1_C8_DetectAESECB.detectAESECB(oracle.encrypt('A'.toByte.multiple(100))))
    Console.println("Detected AES ECB correctly")
    val prefixSize = findPrefixLength(oracle)
    assert(prefixSize == oracle.prefix.length)
    Console.println("Prefix size found: " + prefixSize)
    assert(S2_C9_PCKS7Padding.removePadding(byteAtATimeECBDecryptionHarder(oracle)).deep == oracle.secret.deep)
    Console.println("Secret decrypted: \n" + oracle.secret.toCharString)
  }

  def findPrefixLength(oracle: ECBOracleHarder): Int = {
    def calcMultipleA(i: Int): Array[Byte] = oracle.encrypt('A'.toByte.multiple(i))
    val blockSize: Int = S2_C12_ByteAtATimeECBDecrypt.findBlockLength(oracle)
    val prefixLength = calcMultipleA(0).grouped(blockSize)
                                       .zip(calcMultipleA(1).grouped(blockSize))
                                       .zipWithIndex
                                       .toArray
                                       .takeWhile(e => e._1._1.deep != e._1._2.deep)
                                       .map(_._2)
                                       .apply(0) * blockSize
    (0 until blockSize).find {i =>
      val blocks = calcMultipleA(2 * blockSize + i).grouped(blockSize).toArray
      blocks.zip(blocks.drop(1)).exists(v => v._1.deep == v._2.deep)
    }.map(i => prefixLength + (blockSize - i) % blockSize).getOrElse(0)
  }

  def getNextByteWithPrefix(current: Array[Byte], oracle: ECBOracleHarder): Array[Byte] = {
    val blockSize: Int = S2_C12_ByteAtATimeECBDecrypt.findBlockLength(oracle)
    val prefixSize: Int = findPrefixLength(oracle)
    val blockNumber: Int = (prefixSize + current.length) / blockSize
    val input = 'A'.toByte.multiple(math.abs(blockSize*(blockNumber+1) - prefixSize - 1 - current.length) % blockSize)
    val targetBlock = oracle.encrypt(input).grouped(blockSize).toArray.apply(blockNumber)
    (0 to 255).foreach { c =>
      val fakeBlock = oracle.encrypt(input ++ current ++ Array[Byte](c.toByte)).grouped(blockSize).toArray.apply(blockNumber)
      if (fakeBlock.deep == targetBlock.deep)  {
        return Array[Byte](c.toByte)
      }
    }
    Array.empty[Byte]
  }

  def byteAtATimeECBDecryptionHarder(oracle: ECBOracleHarder): Array[Byte] = {
    val buffer = new ArrayBuffer[Byte]()
    oracle.encrypt().foreach(_ => buffer ++= getNextByteWithPrefix(buffer.toArray, oracle))
    buffer.toArray
  }
}
