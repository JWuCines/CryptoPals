package eu.jiangwu.cryptopals.set3

import scala.util.Random

object S3_C23_CloneMT19937FromOutput {
  def run : Unit = {
    val seed = Random.nextInt(math.pow(2, 32).toInt)
    val rng = new MT19937(seed)
    val MT = (0 until 624).map(i => untemperMT19937(rng.nextInt())).toArray
    val rng2 = new MT19937(0, MT)
    assert((0 to 1000).forall(i => rng.nextInt() == rng2.nextInt()))
    println("The two random generator return same values for the 1000 iterations tested!")
  }

  def untemperMT19937(yIn: Long): Long  = {
    def getMSB(x: Long, n: Int): Long = if (n < 0) 0  else (x >> (31 - n)) & 1
    def setMSB(x: Long, n: Int, b: Long): Long =  x | (b << (31 - n))
    def undoRightShiftXor(y: Long, s: Int): Long = (0 to 32).scanLeft(0L)((z, i) => setMSB(z, i, getMSB(y, i) ^ getMSB(z, i - s))).last

    def getLSB(x: Long, n: Int): Long = if (n < 0) 0 else (x >> n) & 1
    def setLSB(x: Long, n: Int, b: Long): Long = x | (b << n)
    def undoLeftShiftXorAnd(y: Long, s: Int, k: Int): Long = (0 to 32).scanLeft(0L)((z, i) => setLSB(z, i, getLSB(y, i) ^ (getLSB(z, i - s) & getLSB(k, i)))).last

    undoRightShiftXor(undoLeftShiftXorAnd(undoLeftShiftXorAnd(undoRightShiftXor(yIn, 18), 15, 0xefc60000), 7, 0x9d2c5680), 11)
  }
}
