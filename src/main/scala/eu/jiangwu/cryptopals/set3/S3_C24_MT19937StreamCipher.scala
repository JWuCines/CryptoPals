package eu.jiangwu.cryptopals.set3

import eu.jiangwu.cryptopals.set1.S1_C2_FixedXOR

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class MT19937Cipher(key: Int) {
    private val rng = new MT19937(key)

    def encrypt(data: Array[Byte]): Array[Byte] = {
        val keystream: ArrayBuffer[Byte] = ArrayBuffer.empty[Byte]
        while (keystream.size < data.size) {
            keystream.appendAll(rng.nextInt(Int.MaxValue).toString.toCharArray.map(_.toByte))
        }
        S1_C2_FixedXOR.xor(data, keystream.take(data.size).toArray)
    }

    def decrypt(data: Array[Byte]): Array[Byte] = encrypt(data)
}

object S3_C24_MT19937StreamCipher {
    def run: Unit = {
        val seed = Random.nextInt(math.pow(2, 16).toInt - 1)

        val randomPrefix = Random.nextString(Random.nextInt(100)) + ";"
        val known = "myTestKnown"
        val randomSuffix = ";password_reset=true"

        val data = (randomPrefix + known + randomSuffix).getBytes
        val encData = new MT19937Cipher(seed).encrypt(data)
        assert(findMT19937StreamCipherKey(encData, known.getBytes) == seed)
        println("Seed " + seed + " found!")
    }

    def findMT19937StreamCipherKey(data: Array[Byte], known: Array[Byte]): Int =
        (0 until math.pow(2, 16).toInt).find(i => new MT19937Cipher(i).decrypt(data).containsSlice(known)).getOrElse(-1)
}
