package eu.jiangwu.cryptopals
package setone

object S1_C2_FixedXOR {
  def run(in1: String, in2: String, res: String): Unit = {
    val xorRes: String = this.xor(in1.toByteArrayFromHexString, in2.toByteArrayFromHexString).toHexString
    Console.println("Input 1 XOR String: " + in1)
    Console.println("Input 2 XOR String: " + in2)
    Console.println("Calculated XOR String: " + xorRes)
    Console.println("Result XOR: " + res)
    Console.println("Output is same of Result: " + (xorRes == res))
  }

  def xor(in1: Array[Byte], in2: Array[Byte]): Array[Byte] =
    in1.zip(in2)
      .map{ case (i1: Byte, i2: Byte) => xor(i1, i2) }

  def xor(in1: Byte, in2: Byte): Byte =
    ((in1 | in2) & (~in1 | ~in2)).toByte

}
