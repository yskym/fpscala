object Ch5 {
  def streamToList[A](s: Stream[A]): List[A] = s match {
    case Stream.Empty => Nil
    case h #:: t => h :: streamToList(t)
  }

  def streamToList1[A](s: Stream[A]): List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Stream.Empty => acc
      case h #:: t => go(t, h::acc)
    }
    go(s, Nil).reverse
  }

  def take[A](s: Stream[A], n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], n: Int, acc: Stream[A]): Stream[A] = {
      if (n < 1) acc
      else go(s.tail, n - 1, s.head #:: acc)
    }
    go(s, n, Stream.Empty).reverse
  }

  def takeWhile[A](s: Stream[A], p: A => Boolean): Stream[A] = s match {
    case h #:: t if p(h) => h #:: takeWhile(t, p)
    case _ => Stream.Empty
  }

  def foldRight[A,B](s: Stream[A], z: => B)(f: (A, => B) => B): B = s match {
    case h#::t => f(h, foldRight(t, z)(f))
    case _ => z
  }

  def forAll[A](s: Stream[A], p: A => Boolean): Boolean =
    foldRight(s, true)(p(_)&&_)

  def takeWhile1[A](s: Stream[A], p: A => Boolean): Stream[A]  = 
    foldRight(s, Stream[A]())((a, b) => if (p(a)) a #:: b else Stream.Empty)

  def map[A,B](s: Stream[A], f: A => B): Stream[B] = 
    foldRight(s, Stream[B]())((a, b) => f(a) #:: b)

  def filter[A](s: Stream[A], f: A => Boolean): Stream[A] =
    foldRight(s, Stream[A]())((a, b) => if (f(a)) a #:: b else b)

  def append[A](s1: Stream[A], s2: Stream[A]): Stream[A] = 
    foldRight(s1, s2)((a, b) => a #:: b)

  def flatMap[A,B](s: Stream[A], f: A => Stream[B]): Stream[B] = 
    foldRight(s, Stream[B]())((a, b) => append(f(a), b))

  def constant[A](a: A): Stream[A] = a #::constant(a)

  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  def fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map {x => x._1 + x._2}

  def fibs1: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      f0 #:: go(f1, f1 + f0)
    go(0, 1)
  }
      
  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream[A]()
    case Some((a, s)) => a #:: unfold(s)(f)
  }

  def fibs2: Stream[Int] = 
    unfold((0, 1)){ case (f0, f1) => Some(f0, (f1, f0 + f1)) }



}
