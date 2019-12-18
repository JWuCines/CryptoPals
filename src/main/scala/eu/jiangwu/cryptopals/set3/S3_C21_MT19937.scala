package eu.jiangwu.cryptopals
package set3


object S3_C21_MT19937 {
  def run: Unit = {
    val rnd = new MT19937()
    (1 to 10).foreach(i => Console.println("MT19937 rng nextInt attempt " + i + ": " + rnd.nextInt()))
    (1 to 10).foreach(i => Console.println("MT19937 rng nextInt(Int.MaxValue) attempt " + i + ": " + rnd.nextInt(Int.MaxValue)))
    (1 to 10).foreach(i => Console.println("MT19937 rng nextLong attempt " + i + ": " + rnd.nextLong()))
    (1 to 10).foreach(i => Console.println("MT19937 rng nextDouble attempt " + i + ": " + rnd.nextDouble()))
  }
}

// https://gist.github.com/fedesilva/3409244
final class MT19937(seed: Int = 5489, MT: Array[Long] = Array.empty[Long]) {
  private val N = 624
  private val M = 397

  private val MatrixA = 0x9908b0dfL

  private val UpperMask = 0x80000000L
  private val LowerMask = 0x7fffffffL

  private var mti = N + 1

  private val mt: Array[Long] = if(MT.nonEmpty) MT.take(N)
                                else (1 until N).scanLeft(seed.toLong)((p, i) => (1812433253L * (p ^ (p >>> 30)) + i) & 0xffffffffL).toArray.prepended(seed)

  // Generates the next random integer in the sequence
  def nextLong(): Long = {
    var y = 0L

    if (mti >= N) {
      val mag01 = Array(0L, MatrixA)

      var kk = 0
      while (kk < N - M) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
        kk += 1
      }
      while (kk < N - 1) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + (M - N)) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
        kk += 1
      }
      y = (mt(N - 1) & UpperMask) | (mt(0) & LowerMask)
      mt(N - 1) = mt(M - 1) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)

      mti = 0
    }

    // Temper function
    y = mt(mti); mti += 1
    y ^= y >>> 11
    y ^= (y << 7) & 0x9d2c5680L
    y ^= (y << 15) & 0xefc60000L
    y ^= (y >>> 18)

    y
  }

  def nextInt(): Int = {
    nextLong().toInt
  }

  // Generates a random integer in the interval [0, limit)
  def nextInt(limit: Int): Int = {
    // Find shift distance
    val lim = limit.toLong & 0xffffffffL
    var n = -1; var bit = 1L << 32
    while (bit > lim) { n += 1; bit >>>= 1 }

    // Generate integer, take most significant bits; reject while outside interval
    var r = (nextLong & 0xffffffffL) >>> n
    while (r >= lim) { r = (nextLong & 0xffffffffL) >>> n }
    r.toInt
  }

  // Generates a random Double in the interval [0, 1)
  def nextDouble(): Double = {
    val a: Long = (nextInt().toLong & 0xffffffffL) >>> 5
    val b: Long = (nextInt().toLong & 0xffffffffL) >>> 6
    (a * 67108864.0 + b) / 9007199254740992.0
  }
}