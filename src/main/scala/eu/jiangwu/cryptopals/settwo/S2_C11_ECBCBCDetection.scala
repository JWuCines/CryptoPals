package eu.jiangwu.cryptopals
package settwo

import eu.jiangwu.cryptopals.setone.S1_C8_DetectAESECB

import scala.util.Random

object S2_C11_ECBCBCDetection {
    final val BLOCK_SIZE = 16
    final val ECB = "ECB"
    final val CBC = "CBC"
    def run: Unit = {
        val data: Array[Byte] = ("ABCDEF" * 200).getBytes
        (0 to 20).foreach { i =>
            val (mode, encData) = encodeRandomECBCBCwithRandomPadding(data)
            val guess = detectECBCBC(encData)
            assert(mode == guess)
            Console.println("Attempt " + i + " - Encoded mode: " + mode + " - Decoded guess: "+ guess)
        }
    }

    def encodeRandomECBCBCwithRandomPadding(data: Array[Byte]): (String, Array[Byte]) = {
        def generateRandomArrayByte: Array[Byte] = (0 until BLOCK_SIZE).map(i => Random.nextInt(255).toByte).toArray
        def randomBetween5And10: Int = Math.ceil(Random.nextInt(500)*2/100).toInt
        val paddedRightData = addBytes(data, randomBetween5And10, Random.nextInt(255).toByte)
        val paddedLeftData = addBytes(paddedRightData, randomBetween5And10, Random.nextInt(255).toByte, true)
        val paddedData = S2_C9_PCKS7Padding.padding(paddedLeftData, BLOCK_SIZE)
        val randValue = Random.nextInt(1000)
        if (randValue < 230 || (randValue > 689 && randValue < 895))
            (ECB, S2_C10_CBCMode.aesECBEncrypt(paddedData, generateRandomArrayByte))
        else
            (CBC, S2_C10_CBCMode.aesCBCEncrypt(paddedData, generateRandomArrayByte, generateRandomArrayByte))
    }

    def addBytes(data: Array[Byte], size: Int, paddingByte: Byte = 4, top: Boolean = false): Array[Byte] = {
        (if(top) paddingByte.multiple(size) else Array[Byte]()) ++ data ++ (if(!top) paddingByte.multiple(size) else Array[Byte]())
    }

    def detectECBCBC(data: Array[Byte]): String = {
        if(S1_C8_DetectAESECB.detectAESECB(data)) ECB
        else CBC
    }
}
