object Ch6 {
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = positiveInt(rng)
    (n.toDouble / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, r) = rng.nextInt
    val (d, r2) = double(r)
    ((n, d), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 1) (Nil, rng)
    else {
      val (n, r1) = rng.nextInt
      val (ns, r2) = ints(count - 1)(r1)
      (n :: ns, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def positiveMax(n: Int): Rand[Int] = 
    map(_.nextInt)(_.abs%n)

  def double2(rng: RNG): (Double, RNG) = 
    map(positiveInt)(_.toDouble / Int.MaxValue)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }


}

