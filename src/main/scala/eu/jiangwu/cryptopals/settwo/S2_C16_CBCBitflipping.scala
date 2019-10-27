package eu.jiangwu.cryptopals
package settwo

import eu.jiangwu.cryptopals.setone.S1_C2_FixedXOR._

class CBCOracle extends ECBOracleHarder {
  override val prefix = "comment1=cooking%20MCs;userdata=".getBytes
  override val secret = ";comment2=%20like%20a%20pound%20of%20bacon".getBytes
  val iv = generateRandomArrayByte()
  override def getData(data: Array[Byte]): Array[Byte] = prefix ++ data.filter(c => c != ';' && c != '=') ++ secret
  override def encrypt(data: Array[Byte]): Array[Byte] = S2_C10_CBCMode.aesCBCEncrypt(getData(data), key, iv)
  def decrypt(data: Array[Byte]): Array[Byte] = S2_C10_CBCMode.aesCBCDecrypt(data, key, iv)
  def decryptAndCheck(data: Array[Byte], check: Array[Byte]): Boolean = decrypt(data).containsSlice(check)
}

object S2_C16_CBCBitflipping {
  def run = {
    val oracle = new CBCOracle
    val checkValue = ";admin=true".getBytes
    assert(!oracle.decryptAndCheck(oracle.encrypt(checkValue), checkValue))
    Console.println("Using " + checkValue.toCharString + " as value, the oracle will return false for admin value")
    val fakeMessage = 'X'.toByte.multiple(checkValue.length)
    val fakeXORed = xor(checkValue, fakeMessage)
    val encrypted = oracle.encrypt(fakeMessage)
    val xored = xor(encrypted, 0.toByte.multiple(16) ++ fakeXORed)
    assert(oracle.decryptAndCheck(xored, checkValue))
    Console.println("Using " + fakeXORed.toCharString + " as value, the oracle will return true for admin value because of double XOR")
    Console.println("Decoded message: " + oracle.decrypt(xored).toCharString)
  }
}
