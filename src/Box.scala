
case class Box[T](value: T)

object Box {

  // functor
  def map[A, B](f: A => B) = (a: Box[A]) => Box(f(a.value))

  // monad
  def flatMap[A, B](f: A => Box[B]) = (a: Box[A]) => f(a.value)

  // applicative
  def apply[A, B](f: Box[A => B]) = (a: Box[A]) => Box(f.value(a.value))
}