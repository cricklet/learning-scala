package learning.ch9

trait ParseError
trait Parser[A]

trait Parsers[ParserError, Parser[+_]] {
  def char(c: Char): Parser[Char]
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
}
