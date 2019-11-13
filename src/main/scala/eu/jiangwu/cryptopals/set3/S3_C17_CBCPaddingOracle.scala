package eu.jiangwu.cryptopals
package set3

import eu.jiangwu.cryptopals.set1.S1_C1_HexToBase64
import eu.jiangwu.cryptopals.set1.S1_C2_FixedXOR._
import eu.jiangwu.cryptopals.set2.{CBCOracle, S2_C15_PCKS7RemovePadding}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class CBCPaddingOracle extends CBCOracle {
  val strings: List[String] = readListFromResource("set3/17.txt")
  override def getData(data: Array[Byte]): Array[Byte] = S1_C1_HexToBase64.base64Decode(strings(Random.nextInt(10000) % strings.size))
  def decryptAndCheckPadding(data: Array[Byte], iv: Array[Byte]): Boolean = S2_C15_PCKS7RemovePadding.isValidPadding(decrypt(data, iv))
}

object S3_C17_CBCPaddingOracle {
  def run = {
    val oracle: CBCPaddingOracle = new CBCPaddingOracle
    val encData: Array[Byte] = oracle.encrypt()
    Console.println("Encrypted data: " + encData.toHexString)
    val decData: Array[Byte] = decrypt(encData, oracle)
    Console.println("Decrypted data, knowing iv: " + decData.toCharString)
  }

  def lastBlockPreviousByte(encData: Array[Byte], oracle: CBCPaddingOracle, kIV: Array[Byte]): (Byte, Byte) = {
    val kIVlength = kIV.length + 1
    val prefix = generateRandomArrayByte(AES_LENGTH - kIVlength)
    (0 until 128).map(_.toByte).foreach { i =>
      val data: Array[Array[Byte]] = encData.grouped(AES_LENGTH).toArray
      val gIV: Array[Byte] = if(encData.size > 16) data.dropRight(1).last else oracle.iv
      val gVal: Array[Byte] = prefix ++ Array(i) ++ kIV.map(xor(_, kIVlength.toByte))
      val sVal: Array[Byte] = data.dropRight(2).flatten ++ gVal ++ data.last
      Console.println("Length kIV = " + kIVlength + ", i = " + i.toString + ", padding = " + oracle.decryptAndCheckPadding(sVal, gIV))
      if(oracle.decryptAndCheckPadding(sVal, gIV)) {
        val ivPrev: Byte = xor(i, kIVlength.toByte)
        val pPrev: Byte = xor(gIV.apply(gIV.size - kIVlength), ivPrev)
        return (ivPrev, pPrev)
      }
    }
    (0, 0)
  }

  def lastBlock(encData: Array[Byte], oracle: CBCPaddingOracle): Array[Byte] = {
    val kIV: ArrayBuffer[Byte] = ArrayBuffer.empty[Byte]
    (0 until AES_LENGTH).map { _ =>
      val (i, t) = lastBlockPreviousByte(encData, oracle, kIV.toArray.reverse)
      kIV.append(i)
      t
    }.toArray
  }

  def decrypt(encData: Array[Byte], oracle: CBCPaddingOracle): Array[Byte] = {
    (0 until (encData.length / AES_LENGTH)).map { i =>
      lastBlock(encData.dropRight(i * AES_LENGTH), oracle)
    }.reverse.flatten.toArray
  }
}