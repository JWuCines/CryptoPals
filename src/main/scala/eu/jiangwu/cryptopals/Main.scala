package eu.jiangwu.cryptopals

import com.google.gson.Gson
import eu.jiangwu.cryptopals.setone.{S1_C1_HexToBase64, S1_C2_FixedXOR, S1_C3_SingleByteXOR, S1_C4_DetectSingleXOR, S1_C5_RepeatingKeyXOR, S1_C6_BreakRepeatingKeyXOR, S1_C7_AESECBDecrypt, S1_C8_DetectAESECB}
import eu.jiangwu.cryptopals.settwo.{S2_C10_CBCMode, S2_C11_ECBCBCDetection, S2_C12_ByteAtATimeECBDecrypt, S2_C13_ECBCutAndPaste, S2_C9_PCKS7Padding}

import scala.io.Source

case class Challenge(index: Int, name: String, challenges: Array[Challenge] = Array.empty[Challenge])
case class Challenges(challenges: Array[Challenge])

object Main {

  def main(args: Array[String]): Unit = {
    val challenges: Array[Challenge] = readChallengesFromJson
    Console.println("Choose the set: ")
    challenges.foreach(c => Console.println("\t" + c.index + " - " + c.name))
    val set: Int = io.StdIn.readInt().ensuring(v => challenges.map(_.index).contains(v))
    Console.println("Choose the challenge: ")
    challenges.filter(_.index == set).map(s => s.challenges.map(c => Console.println("\t" + c.index + " - " + c.name)))
    val chIndex = io.StdIn.readInt().ensuring(v => challenges.filter(_.index == set).exists(_.challenges.exists(_.index == v)))
    set match {
      case 1 =>
        chIndex match {
          case 1 =>
            S1_C1_HexToBase64.run
          case 2 =>
            S1_C2_FixedXOR.run
          case 3 =>
            S1_C3_SingleByteXOR.run
          case 4 =>
            S1_C4_DetectSingleXOR.run
          case 5 =>
            S1_C5_RepeatingKeyXOR.run
          case 6 =>
            val testString1: String = "this is a test"
            val testString2: String = "wokka wokka!!!"
            assert(S1_C6_BreakRepeatingKeyXOR.hamming(testString1, testString2) == 37)
            S1_C6_BreakRepeatingKeyXOR.run
          case 7 =>
            val key: String = "YELLOW SUBMARINE"
            assert(key.length == 16)
            S1_C7_AESECBDecrypt.run(key)
          case 8 =>
            S1_C8_DetectAESECB.run
        }
      case 2 =>
        chIndex match {
          case 9 =>
            S2_C9_PCKS7Padding.run
          case 10 =>
            S2_C10_CBCMode.run
          case 11 =>
            S2_C11_ECBCBCDetection.run
          case 12 =>
            S2_C12_ByteAtATimeECBDecrypt.run
          case 13 =>
            S2_C13_ECBCutAndPaste.run
        }
    }
  }

  def readChallengesFromJson: Array[Challenge] = {
    val json: String = Source.fromResource("challenges.json").getLines.mkString("\n").stripMargin
    if(json.length > 0) {
      val gson = new Gson()
      return gson.fromJson(json, classOf[Challenges]).challenges
    }
    Array.empty[Challenge]
  }

}