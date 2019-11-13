package eu.jiangwu.cryptopals
package set2

import eu.jiangwu.cryptopals.set1.S1_C2_FixedXOR._

class CBCOracle extends ECBOracleHarder {
  override val prefix = "comment1=cooking%20MCs;userdata=".getBytes
  override val secret = ";comment2=%20like%20a%20pound%20of%20bacon".getBytes
  val iv = generateRandomArrayByte()
  override def getData(data: Array[Byte]): Array[Byte] = prefix ++ data.filter(c => c != ';' && c != '=') ++ secret
  override def encrypt(data: Array[Byte]): Array[Byte] = S2_C10_CBCMode.aesCBCEncrypt(getData(data), key, iv)
  def decrypt(data: Array[Byte], iv: Array[Byte]): Array[Byte] = S2_C10_CBCMode.aesCBCDecrypt(data, key, iv)
  def decrypt(data: Array[Byte]): Array[Byte] = decrypt(data, iv)
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
    Console.println("Decoded message: " + S2_C9_PCKS7Padding.removePadding(oracle.decrypt(xored)).toCharString)
  }
}
/*
    val desiredValue: Array[Byte] = checkValue.map(x => if(x == ';' || x == '=') '?'.toByte else x)
    val blockSize: Int = S2_C12_ByteAtATimeECBDecrypt.findBlockLength(oracle)
    val prefixLength: Int = S2_C14_ByteAtATimeECBDecryptHarder.findPrefixLength(oracle)
    val prefixBlocksLength: Int = math.ceil(prefixLength.toDouble / blockSize).toInt * blockSize
    val prefixPadding: Int = prefixBlocksLength - prefixLength
    val checkValuePadding: Int = blockSize - (checkValue.length % blockSize)
    val finalText: Array[Byte] = '?'.toByte.multiple(prefixPadding + checkValuePadding) ++ desiredValue
    val encrypted: Array[Byte] = oracle.encrypt(finalText)
    // XOR of a XOR is equal 0, so let's try it to do it in the prefix
    def getIndexFor(c: Char): Int = prefixBlocksLength - checkValue.length - checkValue.indexOf(c)
    val indexForSemiColon: Int = getIndexFor(';')
    val semiColonValue: Byte = xor(xor(encrypted(indexForSemiColon), '?'), ';')
    val indexForEqual: Int  = getIndexFor('=')
    val equalValue: Byte = xor(xor(encrypted(indexForEqual), '?'), '=')
    val forcedEncrypted: Array[Byte]  = encrypted.updated(indexForSemiColon, semiColonValue).updated(indexForEqual, equalValue)
    Console.println("Decoded message: " + oracle.decrypt(forcedEncrypted).toCharString)
    assert(oracle.decryptAndCheck(forcedEncrypted, checkValue))
    Console.println("Using " + forcedEncrypted.toCharString + " as value, the oracle will return true for admin value because of double XOR")
    Console.println("Decoded message: " + oracle.decrypt(forcedEncrypted).toCharString)
 */