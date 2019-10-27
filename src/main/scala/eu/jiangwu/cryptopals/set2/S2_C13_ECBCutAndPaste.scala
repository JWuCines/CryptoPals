package eu.jiangwu.cryptopals
package set2

import eu.jiangwu.cryptopals.set1.S1_C7_AESECBDecrypt

case class Profile(email: String, uid: Int = 10, role: String = "user") {
  def toCookieString: String = "email=" + email + "&uid=" + uid + "&role=" + role
}

object Profile {
  def apply(in: String): Profile = {
    val input = in.split("&").map(_.split("=")).map{case Array(k, v) => k -> v}.toMap
    Profile(input("email"), input("uid").toInt, input("role"))
  }
}

class ECBProfile {
  val key = generateRandomArrayByte(16)
  def encrypt(data: String): Array[Byte] = S2_C10_CBCMode.aesECBPKCS5Encrypt(profileFor(data).getBytes, key)
  def decrypt(data: Array[Byte]): String = S2_C9_PCKS7Padding.removePadding(S1_C7_AESECBDecrypt.aesECBPKCS5Decrypt(data, key)).toCharString
  def profileFor(input: String): String = Profile(input.split("&")(0).split("=")(0), 10, "user").toCookieString
}

object S2_C13_ECBCutAndPaste {
  val profile = new ECBProfile
  def run: Unit = {
    val input = "email@example.com"
    val output = "email="+input+"&uid=10&role=user"
    assert(profile.profileFor(input) == output)
    Console.println("Input " + input + " should return "+ output)
    val input2 = "email@example.com&asdaga=wjiqogoq"
    assert(profile.profileFor(input2) == output)
    Console.println("Input " + input2 + " should return "+ output)
    val (usedText, encData) = ecbCutAndPaste(profile)
    val decProfile = profile.decrypt(encData)
    Console.println(decProfile)
    assert(Profile(decProfile).role == "admin")
    Console.println("Email length requested: " + usedText.length)
    Console.println("Decoded profile data: " + decProfile)
  }

  def ecbCutAndPaste(oracle: ECBProfile): (String, Array[Byte]) = {
    val prefixLength = AES_LENGTH - "email=".length
    val adminBlock = oracle.encrypt("x" * prefixLength + S2_C9_PCKS7Padding.padding("admin".getBytes, AES_LENGTH).toCharString)
                            .grouped(AES_LENGTH).slice(1, 2).toArray.flatten
    val loginLength = "email=".length + "&uid=10&role=".length
    val blocks = (loginLength / AES_LENGTH) + 1
    val loginText = "a" * (blocks * AES_LENGTH - loginLength)
    val loginBlocks = oracle.encrypt(loginText).grouped(AES_LENGTH).take(blocks).toArray.flatten
    (loginText, loginBlocks ++ adminBlock)
  }
}
