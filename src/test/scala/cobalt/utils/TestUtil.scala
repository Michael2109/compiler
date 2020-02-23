package cobalt.utils

import fastparse.{P, Parsed}

object TestUtil {

  def parse(text: String, parser: P[_] => P[_]) = {

    fastparse.parse(text, parser) match {
      case Parsed.Success(value, _) => value
      case Parsed.Failure(a, b, c) => throw new Exception("Failed parsing:" + a + ":" + b + ":" + c)
    }
  }
}
