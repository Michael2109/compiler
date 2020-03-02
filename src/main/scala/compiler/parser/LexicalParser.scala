package compiler.parser

import fastparse.NoWhitespace._
import fastparse._

object LexicalParser {

  def kw[_: P](s: String) = s ~ !(letter | digit | "_")

  def comment[_: P] = P("#" ~ CharsWhile(_ != '\n', 0))

  def wscomment[_: P] = P((CharsWhileIn(" \n") | LexicalParser.comment | "\\\n").rep)

  def nonewlinewscomment[_: P] = P((CharsWhileIn(" ") | LexicalParser.comment | "\\\n").rep)

  def identifier[_: P]: P[String] =
    P((letter | "_") ~ (letter | digit | "_").rep).!.filter(!keywordList.contains(_))

  def letter[_: P] = P(lowercase | uppercase)

  def lowercase[_: P] = P(CharIn("a-z"))

  def uppercase[_: P] = P(CharIn("A-Z"))

  def digit[_: P] = P(CharIn("0-9"))

  def keywordList = Set(
    "and", "del", "from", "not", "while",
    "as", "elif", "global", "or", "with",
    "assert", "else", "if", "pass", "yield",
    "break", "except", "import",
    "class", "exec", "in", "raise",
    "continue", "finally", "is", "return",
    "for", "lambda", "try", "mutable",
    "let"
  )

  def stringliteral[_: P]: P[String] = P(stringprefix.? ~ (longstring | shortstring))

  def stringprefix[_: P]: P0 = P(
    "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR"
  )

  def shortstring[_: P]: P[String] = P(shortstring0("'") | shortstring0("\""))

  def shortstring0[_: P](delimiter: String) = P(delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)

  def shortstringitem[_: P](quote: String): P0 = P(shortstringchar(quote) | escapeseq)

  def shortstringchar[_: P](quote: String): P0 = P(CharsWhile(!s"\\\n${quote(0)}".contains(_)))

  def longstring[_: P]: P[String] = P(longstring0("'''") | longstring0("\"\"\""))

  def longstring0[_: P](delimiter: String) = P(delimiter ~ longstringitem(delimiter).rep.! ~ delimiter)

  def longstringitem[_: P](quote: String): P0 = P(longstringchar(quote) | escapeseq | !quote ~ quote.take(1))

  def longstringchar[_: P](quote: String): P0 = P(CharsWhile(!s"\\${quote(0)}".contains(_)))

  def escapeseq[_: P]: P0 = P("\\" ~ AnyChar)

  def negatable[T, _: P](p: => P[T])(implicit ev: Numeric[T]): P[T] = (("+" | "-").?.! ~ p).map {
    case ("-", i) => ev.negate(i)
    case (_, i) => i
  }

  def longinteger[_: P]: P[BigInt] = P(integer ~ ("l" | "L"))

  def integer[_: P]: P[BigInt] = negatable(P(octinteger | hexinteger | bininteger | decimalinteger))

  def decimalinteger[_: P]: P[BigInt] = P(nonzerodigit ~ digit.rep | "0").!.map(scala.BigInt(_))

  def octinteger[_: P]: P[BigInt] = P("0" ~ ("o" | "O") ~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).!).map(scala.BigInt(_, 8))

  def hexinteger[_: P]: P[BigInt] = P("0" ~ ("x" | "X") ~ hexdigit.rep(1).!).map(scala.BigInt(_, 16))

  def bininteger[_: P]: P[BigInt] = P("0" ~ ("b" | "B") ~ bindigit.rep(1).!).map(scala.BigInt(_, 2))

  def nonzerodigit[_: P]: P0 = P(CharIn("1-9"))

  def octdigit[_: P]: P0 = P(CharIn("0-7"))

  def bindigit[_: P]: P0 = P("0" | "1")

  def hexdigit[_: P]: P0 = P(digit | CharIn("a-f", "A-F"))


  def floatnumber[_: P]: P[BigDecimal] = negatable(P(pointfloat | exponentfloat))

  def pointfloat[_: P]: P[BigDecimal] = P(intpart.? ~ fraction | intpart ~ ".").!.map(BigDecimal(_))

  def exponentfloat[_: P]: P[BigDecimal] = P((intpart | pointfloat) ~ exponent).!.map(BigDecimal(_))

  def intpart[_: P]: P[BigDecimal] = P(digit.rep(1)).!.map(BigDecimal(_))

  def fraction[_: P]: P0 = P("." ~ digit.rep(1))

  def exponent[_: P]: P0 = P(("e" | "E") ~ ("+" | "-").? ~ digit.rep(1))


  def imagnumber[_: P] = P((floatnumber | intpart) ~ ("j" | "J"))
}
