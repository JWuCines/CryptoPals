package eu.jiangwu.cryptopals
package setone

object S1_C5_RepeatingKeyXOR {

  def run(in: String, key: String, result: String) = {
    Console.println("Input text: \n" + in)
    Console.println("Correct result text: \n\t" + result)
    val calcValue = keyXOR(in, key).toHexString
    Console.println("Return XORed: \n\t" + calcValue)
    Console.println("Return XORed is equal to result: " + (result == calcValue))
  }

  def keyXOR(s: String, key: String): Array[Byte] = {
    val rKey = (key * math.ceil(s.length.toDouble / key.length).toInt).take(s.length)
    S1_C2_FixedXOR.xor(s.getBytes, rKey.getBytes)
  }
}
