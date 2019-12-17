package eu.jiangwu.cryptopals.set3

import scala.util.Random

object S3_C22_CrackMT19937 {
  def run: Unit = {
    val rnd = new MT19937(159753)
    val rnd2 = new MT19937(159753)
    assert((1 to 10).map(i => rnd.nextInt()).diff((1 to 10).map(i => rnd2.nextInt())).isEmpty)

    val time = System.currentTimeMillis.toInt
    val fakeDelta = Random.between(40, 1000)
    val exSeed = time + fakeDelta
    val randNb = new MT19937(exSeed).nextInt()
    val outTime = time + fakeDelta + Random.between(40, 1000)

    val res = (40 to 1000).find(i => new MT19937(outTime - i).nextInt() == randNb)
    assert(res.isDefined)
    println("Seed used = " + exSeed + " - Seed found = " + res.map(outTime - _).get)
  }
}