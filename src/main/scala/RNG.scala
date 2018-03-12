trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECEDL + 0xBL) &
                  ((1L << 48) - 1)
      ((seed >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }
  
}
