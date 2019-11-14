package eu.jiangwu.cryptopals
package set3

import eu.jiangwu.cryptopals.set1.S1_C1_HexToBase64
import eu.jiangwu.cryptopals.set1.S1_C2_FixedXOR._
import eu.jiangwu.cryptopals.set2.{CBCOracle, S2_C15_PCKS7RemovePadding, S2_C9_PCKS7Padding}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class CBCPaddingOracle extends CBCOracle {
  val strings: List[String] = readListFromResource("set3/17.txt")
  override def getData(data: Array[Byte]): Array[Byte] = strings(Random.nextInt(10000) % strings.size).getBytes
  def decryptAndCheckPadding(data: Array[Byte], iv: Array[Byte]): Boolean = S2_C15_PCKS7RemovePadding.isValidPadding(decrypt(data, iv))
}

object S3_C17_CBCPaddingOracle {
  def run = {
    val oracle: CBCPaddingOracle = new CBCPaddingOracle
    val encData: Array[Byte] = oracle.encrypt()
    Console.println("Encrypted data: " + encData.toHexString)
    val decData: String = attackPaddingOracle(encData, oracle).toCharString
    Console.println("Decrypted data, knowing iv: " + decData)
    Console.println("Decoded and decrypted data, knowing iv: " + S1_C1_HexToBase64.base64Decode(decData))
  }

  def forcePreviousBlock(iv: Array[Byte], b: Byte, padLen: Int, kPlain: Array[Byte]): Array[Byte] = {
    def calcChar(i: Int, by: Byte): Byte = xor(xor(iv(i), by), padLen.toByte)

    val indexChar = iv.length - padLen
    iv.take(indexChar) ++ Array(calcChar(indexChar, b)) ++
      (AES_LENGTH - padLen + 1 until AES_LENGTH).zipWithIndex.map( v => calcChar(v._1, kPlain(v._2)))
  }

  def attackPaddingOracle(encData: Array[Byte], oracle: CBCPaddingOracle): Array[Byte] = {
    S2_C9_PCKS7Padding.removePadding(
      (Array(oracle.iv) ++ encData.grouped(AES_LENGTH)).sliding(2).flatMap { case blocks: Array[Array[Byte]] =>
      val plainBlock: ArrayBuffer[Byte] = ArrayBuffer.empty[Byte]
      (0 until AES_LENGTH).reverse.toArray.foreach { i =>
        val padLen = plainBlock.size + 1
        val v = (0 until 128).map(_.toByte).toArray.flatMap { c =>
          if (oracle.decryptAndCheckPadding(blocks(1), forcePreviousBlock(blocks(0), c, padLen, plainBlock.toArray))) Option(c)
          else None
        }.flatMap { b =>
          (0 until 128).map(_.toByte).flatMap { c =>
            if (oracle.decryptAndCheckPadding(blocks(1), forcePreviousBlock(blocks(0), c, padLen + 1, Array(b) ++ plainBlock))) Option(b)
            else None
          }
        }
        plainBlock.prepend(v.head)
      }
      plainBlock
    }.toArray)
  }
}