package eu.jiangwu.cryptopals
package set1

object S1_C5_RepeatingKeyXOR {

  def run = {
    val in: String = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val result: String = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632427276527" +
      "2a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    val key: String = "ICE"
    Console.println("Input text: \n" + in)
    Console.println("Correct result text: \n\t" + result)
    val calcValue = keyXOR(in.getBytes, key).toHexString
    Console.println("Return XORed: \n\t" + calcValue)
    Console.println("Return XORed is equal to result: " + (result == calcValue))
  }

  def keyXOR(s: Array[Byte], key: String): Array[Byte] = {
    val rKey = (key * math.ceil(s.length.toDouble / key.length).toInt).take(s.length)
    S1_C2_FixedXOR.xor(s, rKey.getBytes)
  }
}
