package eu.jiangwu.cryptopals

import com.google.gson.Gson
import eu.jiangwu.cryptopals.setone.{S1_C1_HexToBase64, S1_C2_FixedXOR, S1_C3_SingleByteXOR, S1_C4_DetectSingleXOR, S1_C5_RepeatingKeyXOR, S1_C6_BreakRepeatingKeyXOR, S1_C7_AESECBDecrypt}

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
            val hex: String = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" // INPUT
            val result: String = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" // OUTPUT
            S1_C1_HexToBase64.run(hex, result)
          case 2 =>
            val x1: String = "1c0111001f010100061a024b53535009181c"
            val x2: String = "686974207468652062756c6c277320657965"
            val res: String = "746865206b696420646f6e277420706c6179"
            S1_C2_FixedXOR.run(x1, x2, res)
          case 3 =>
            val x1: String = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
            S1_C3_SingleByteXOR.run(x1)
          case 4 =>
            S1_C4_DetectSingleXOR.run()
          case 5 =>
            val input: String = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
            val result: String = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632427276527" +
                                 "2a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
            val key: String = "ICE"
            S1_C5_RepeatingKeyXOR.run(input, key, result)
          case 6 =>
            val testString1: String = "this is a test"
            val testString2: String = "wokka wokka!!!"
            assert(S1_C6_BreakRepeatingKeyXOR.hamming(testString1, testString2) == 37)
            S1_C6_BreakRepeatingKeyXOR.run()
          case 7 =>
            val key: String = "YELLOW SUBMARINE"
            S1_C7_AESECBDecrypt.run(key)
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