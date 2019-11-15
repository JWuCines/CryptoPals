package eu.jiangwu.cryptopals
package set2

import eu.jiangwu.cryptopals.set1.S1_C8_DetectAESECB

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object S2_C11_ECBCBCDetection {
    final val BLOCK_SIZE = AES_LENGTH
    final val ECB = "ECB"
    final val CBC = "CBC"
    def run: Unit = {
        val data: Array[Byte] = ("ABCDEFGHILMNOPQRSTUVZ" * 200).getBytes
        (0 to 20).foreach { i =>
            val (mode, encData) = encodeRandomECBCBCwithRandomPadding(data)
            val guess = detectECBCBC(encData)
            assert(mode == guess)
            Console.println("Attempt " + i + " - Encoded mode: " + mode + " - Decoded guess: "+ guess)
        }
    }

    def encodeRandomECBCBCwithRandomPadding(data: Array[Byte]): (String, Array[Byte]) = {
        def randomBetween5And10: Int = Math.ceil(Random.nextInt(500)*2/100).toInt
        val fakeData = generateRandomArrayByte(randomBetween5And10) ++ data ++ generateRandomArrayByte(randomBetween5And10)
        val randValue = Random.nextInt(1000)
        if (randValue < 230 || (randValue > 689 && randValue < 895))
            (ECB, S2_C10_CBCMode.aesECBPKCS5Encrypt(fakeData, generateRandomArrayByte(BLOCK_SIZE)))
        else
            (CBC, S2_C10_CBCMode.aesCBCEncrypt(fakeData, generateRandomArrayByte(BLOCK_SIZE), generateRandomArrayByte(BLOCK_SIZE)))
    }

    def detectECBCBC(data: Array[Byte]): String = {
        if(S1_C8_DetectAESECB.detectAESECB(data)) ECB
        else CBC
    }
}
