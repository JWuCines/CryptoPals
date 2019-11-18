package eu.jiangwu

import scala.io.Source
import scala.util.Random

package object cryptopals {
  val AES_LENGTH = 16

  def readArrayFromResource(path: String): Array[String] = {
    Source.fromResource(path).getLines.map(_.stripMargin).toArray
  }

  def generateRandomArrayByte(size: Int = AES_LENGTH): Array[Byte] = (0 until size).map(i => Random.nextInt(128).toByte).toArray

  def scoreWords(str: Array[Byte]): Double = {
    val CHARACTER_FREQ: Map[Char, Double] = Map(
      'a'-> 0.0651738, 'b'-> 0.0124248, 'c'-> 0.0217339, 'd'-> 0.0349835, 'e'-> 0.1041442, 'f'-> 0.0197881, 'g'-> 0.0158610,
      'h'-> 0.0492888, 'i'-> 0.0558094, 'j'-> 0.0009033, 'k'-> 0.0050529, 'l'-> 0.0331490, 'm'-> 0.0202124, 'n'-> 0.0564513,
      'o'-> 0.0596302, 'p'-> 0.0137645, 'q'-> 0.0008606, 'r'-> 0.0497563, 's'-> 0.0515760, 't'-> 0.0729357, 'u'-> 0.0225134,
      'v'-> 0.0082903, 'w'-> 0.0171272, 'x'-> 0.0013692, 'y'-> 0.0145984, 'z'-> 0.0007836, ' '-> 0.1918182
    )
    str.map(c => CHARACTER_FREQ.getOrElse(c.toChar.toLower, 0d)).sum
  }

  implicit class ArrayByteTransformation(bytes: Array[Byte]) {
    def toHexString: String = {
      bytes.map(b => String.format("%02x", Byte.box(b))).mkString
    }

    def toCharString: String = {
      bytes.map(c => if(c == 10 || c == 13 || (c > 31 && c < 127)) c.toChar else "\\x" + Array(c).toHexString).mkString
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
