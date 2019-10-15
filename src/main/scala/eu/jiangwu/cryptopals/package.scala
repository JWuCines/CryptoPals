package eu.jiangwu

import scala.io.Source
import scala.util.Random

package object cryptopals {
  val AES_LENGTH = 16

  def readListFromResource(path: String): List[String] = {
    Source.fromResource(path).getLines.map(_.stripMargin).toList
  }

  def generateRandomArrayByte(size: Int = AES_LENGTH): Array[Byte] = (0 until size).map(i => Random.nextInt(255).toByte).toArray

  implicit class ArrayByteTransformation(bytes: Array[Byte]) {
    def toHexString: String = {
      bytes.map(b => String.format("%02x", Byte.box(b))).mkString
    }

    def toCharString: String = {
      new String(bytes)
    }
  }

  implicit class ByteTransformation(byte: Byte) {
    def multiple(times: Int): Array[Byte] = (byte.toChar.toString * times).getBytes
  }

  implicit class StringTransformation(str: String) {
    def toHexString: String = {
      str.toCharArray.map(_.toByte).toHexString
    }

    def toByteArrayFromHexString: Array[Byte] = {
      str.grouped(2).toArray.map(Integer.parseInt(_, 16).toByte)
    }

    def toASCIIFromHexString: String = {
      str.toByteArrayFromHexString.map(_.toChar).mkString
    }
  }
}
