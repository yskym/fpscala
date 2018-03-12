
object Ch3List {
  // Ex 2. Remove first element of a list
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: t => t
  }

  // Ex 3. Remove first n elements of a list
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 1) l
    else drop(tail(l), n - 1)
  }

  // Ex 4.
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case h :: t => if (f(h)) dropWhile(t, f) else h::dropWhile(t, f)
  }

  // Ex 5.
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => Nil
    case _::t => x::t
  }

  // Ex 6. all but the last element
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h::Nil => Nil
    case h::t => h::init(t)
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case h::t => f(h, foldRight(t, z)(f))
  }

  // Ex 9.
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, y) => y + 1)
  }

  // Ex 10.
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case h::t => foldLeft(t, f(z, h))(f)
  }

  // Ex 11.
  def sum(l: List[Int]): Int = 
    foldLeft(l, 0)(_+_)

  def prod(l: List[Double]): Double = 
    foldLeft(l, 1.0)(_*_)

  def length2[A](l: List[A]): Int = 
    foldLeft(l, 0)((y, _) => y + 1)

  // Ex 12.
  def reverseList2[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((xs: List[A], x: A) => x::xs)

  // Ex 13.
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b:B) => b)((a, g) => (b => g(f(b, a))))(z)
  }

  // Ex 14.
  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((x, l) => x::l)
  }

  def concat[A](ll: List[List[A]]): List[A] = {
    foldLeft(ll, Nil: List[A])(append)
  }

  def increment(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((a, b) => (a + 1)::b)
  }
  
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])(f(_)::_)
  }

  def filter[A](l: List[A])(f: A => Boolean) = {
    foldRight(l, Nil: List[A])((a,b) => {
      if (f(a)) a::b
      else b
    })
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    //foldRight(l, Nil: List[B])((a, bs) => append(f(a), bs))
    concat(map(l)(f))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) a::Nil else Nil)
  }

  def componentAdd(l1: List[Int], l2: List[Int]): List[Int] = {
    if (l1 == Nil) Nil
    else (l1.head + l2.head)::componentAdd(l1.tail, l2.tail)
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h1::t1, h2::t2) => f(h1, h2)::zipWith(t1, t2)(f)
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = 
    (l, sub) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (lh::lt, subh::subt) => {
        if (lh == subh) hasSubsequence(lt, subt)
        else hasSubsequence(lt, sub)
      }
  }



}
