
package fpinscala.laziness


trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) Cons(h, () => t().take(n-1)) else Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) t().drop(n-1) else this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      if (p(h())) Cons(h, () => t().takeWhile(p))
      else Empty
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[AA>:A](a: AA): Stream[AA] = foldRight(Stream(a))(Stream.cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = map(f(_).headOption.get)

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def map2[B](f: A => B): Stream[B] = Stream.unfold(this){
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def take2(n: Int): Stream[A] = Stream.unfold((this, n)){
    case (Cons(h, t), cnt) => if (cnt > 0) Some((h(), (t(), cnt-1))) else None
    case _ => None
  }

  def takeWhile3(p: A => Boolean): Stream[A] = Stream.unfold(this){
    case Cons(h, t) => if (p(h())) Some((h(), t())) else None
    case _ => None
  }

  def zipWith[AA>:A](s2: Stream[AA])(f: (AA, AA) => AA): Stream[AA] = Stream.unfold((this, s2)){
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h, t), _) => Some((Some(h()), None), (t(), Empty))
    case (_, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case (_, _) => None
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  def fibs2: Stream[Int] = unfold((0, 1)){case (a, b)  => Some(a, (b, a+b))}

  def ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some((x, x+1)))



}
