
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = 
    flatMap(x => Right(f(x)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
    
  def orElse[EE >: E,B >: A] (b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(e) => Right(e)
  }
  def map2[EE >: E, B, C](b: Either[EE, B]) (f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a1), Right(a2)) => Right(f(a1, a2))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either{
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]])((a, b) => a.flatMap(x => b.map(list => x::list)))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as.foldRight(Right(Nil): Either[E, List[B]])((a, b) => f(a).map2(b)(_ :: _))
    
}
