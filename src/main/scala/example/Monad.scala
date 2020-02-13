package example

trait Monad[T] {

  def flatMap[U](f: T => Monad[U]): Monad[U]

  def unit[T](x:T): Monad[T]
}
