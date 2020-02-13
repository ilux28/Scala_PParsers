package parser_monad

class Parser[T, Src](private val p: Src => (T, Src)) {
  def flatMap[M](f: T => Parser[M,Src]): Parser[M, Src] =
    Parser { src =>
      val (word, rest) = p(src)
      f(word).p(rest)
    }
  def map[M](f: T => M): Parser[M, Src] =
    Parser { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }
  def parse(src: Src): T = p(src)._1
}

object Parser {
  def apply[T, Src](f: Src => (T, Src)) = new Parser(f)
}
