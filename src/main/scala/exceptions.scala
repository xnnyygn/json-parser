sealed abstract class AbstractJsonParseException(text: String, current: Int) extends RuntimeException {
  // TODO locate by line and column
}

class NotABooleanException(text: String, current: Int) extends AbstractJsonParseException(text, current)

class UnclosedStringException(text: String, current: Int) extends AbstractJsonParseException(text, current)

/* merge with abstract JSON parse exception */
class UnexpectedPrefixException(prefix: Char) extends RuntimeException("unexpected prefix [" + prefix + "]")

class ColonNotFoundException(text: String, current: Int) extends AbstractJsonParseException(text, current)
