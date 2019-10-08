package eu.jiangwu.cryptopals
package setone

import java.util.Base64
import scala.collection.mutable.ArrayBuffer

object S1_C1_HexToBase64 {
  /*
    Reference: http://base64.sourceforge.net/b64.c
   */
  private val cb64: Array[Char] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray
  private val cd64: Array[Char] = "|$$$}rstuvwxyz{$$$$$$$>?@ABCDEFGHIJKLMNOPQRSTUVW$$$$$$XYZ[\\]^_`abcdefghijklmnopq".toCharArray

  def run: Unit = {
    val hex: String = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" // INPUT
    val result: String = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" // OUTPUT
    val calc64: String = base64Encode(hex.toByteArrayFromHexString)
    val dec64: String = base64Decode(result).toHexString
    Console.println("Input HEX String: " + hex)
    Console.println("Output BASE64: " + calc64)
    Console.println("Result BASE64: " + result)
    Console.println("Output is same of Result: " + (calc64 == result))
    Console.println("Result BASE64 decoded: " + dec64)
    Console.println("Output is same of Result decoded: " + (dec64 == hex))
  }

  def base64Encode(array: Array[Byte]): String = {
    array.grouped(3).toArray.flatMap { in =>
      val out: ArrayBuffer[Char] = new ArrayBuffer[Char]()
      val len: Int = in.length
      out += cb64(in(0) >> 2) // Remove the last 2 element
      out += cb64(((in(0) & 0x03) << 4) | ((in(1) & 0xf0) >> 4))
      out += { if(len > 1) { cb64(((in(1) & 0x0f) << 2) | ((in(2) & 0xc0) >> 6)) } else '=' }
      out += { if(len > 2) { cb64(in(2) & 0x3f)} else '=' }
      out
    }.mkString
  }

  def base64Decode(str: String): Array[Byte] = {
    val result = str.getBytes.flatMap(
      Option(_).filterNot(e => e < 43 || e > 122).map(e => cd64(e - 43)).filter(_ != '$').map(_ - 61)
    ).map(_ - 1).grouped(4).toArray.flatMap { in =>
      val inFull: ArrayBuffer[Int] = ArrayBuffer(in: _*)
      while (inFull.length < 4) inFull += 0.toChar
      val out: ArrayBuffer[Byte] = new ArrayBuffer[Byte]()
      out += (inFull(0) << 2 | inFull(1) >> 4).toByte
      out += (inFull(1) << 4 | inFull(2) >> 2).toByte
      out += (((inFull(2) << 6) & 0xc0) | inFull(3)).toByte
      out
    }.reverse.dropWhile(_ == 0).reverse
    assert(result.deep == base64DecodeJava(str).deep)
    result
  }

  private def base64DecodeJava(str: String): Array[Byte] = Base64.getDecoder.decode(str)
}
