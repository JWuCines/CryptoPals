package eu.jiangwu.cryptopals
package set1

object S1_C2_FixedXOR {
  def run: Unit = {
    val in1: String = "1c0111001f010100061a024b53535009181c"
    val in2: String = "686974207468652062756c6c277320657965"
    val res: String = "746865206b696420646f6e277420706c6179"
    val xorRes: String = this.xor(in1.toByteArrayFromHexString, in2.toByteArrayFromHexString).toHexString
    Console.println("Input 1 XOR String: " + in1)
    Console.println("Input 2 XOR String: " + in2)
    Console.println("Calculated XOR String: " + xorRes)
    Console.println("Result XOR: " + res)
    Console.println("Output is same of Result: " + (xorRes == res))
  }

  def xor(in1: Array[Byte], in2: Array[Byte]): Array[Byte] =
    in1.zipAll(in2, 0.toByte, 0.toByte)
      .map{ case (i1: Byte, i2: Byte) => xor(i1, i2) }

  def xor(in1: Byte, in2: Byte): Byte =
    ((in1 | in2) & (~in1 | ~in2)).toByte

}
