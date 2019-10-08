package eu.jiangwu

import scala.io.Source

package object cryptopals {
  def readListFromResource(path: String): List[String] = {
    Source.fromResource(path).getLines.map(_.stripMargin).toList
  }

  implicit class ByteArrayToHex(bytes: Array[Byte]) {
    def toHexString: String = {
      bytes.map(b => String.format("%02x", Byte.box(b))).mkString
    }

    def toCharString: String = {
      new String(bytes)
    }
  }

  implicit class StringAndHex(str: String) {
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
