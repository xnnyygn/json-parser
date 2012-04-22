import java.lang.{UnsupportedOperationException => USE}
object JsonParser {  
	/* parse JSON text and return a JSON object */
  def parse(text: String): JsonObject[_] = parse(text, 0)._1
	/* parse text from specified postiton 
	and return a tuple with JSON object and next position */
	private def parse(text: String, current: Int): (JsonObject[_], Int) = {
	  val charAndPos = firstNotWhiteChar(text, current)
		val newPos = charAndPos._2
		charAndPos._1 match {
			case '"' => parseString(text, newPos)
			case 'f' | 't' => parseBoolean(text, newPos)
			case '[' => parseList(text, newPos)
			case '{' => parseMap(text, newPos)
			case d if isDigit(d) => parseNumber(text, newPos)
			case prefix => throw new UnexpectedPrefixException(prefix)
		}
	}
	/* read character and find first not white one
	and return a tuple with char and position */
	private def firstNotWhiteChar(text: String, current: Int): (Char, Int) = {
	  val position = skipUntil(text, current, {c => !Character.isWhitespace(c)})
	  (getCharAt(text, position), position)
	}
	private def getCharAt(text: String, index: Int): Char = {
	  if(index >= text.length) '\0'
		else text(index)
	}
	private def skipUntil(text: String, current: Int, until: Char => Boolean): Int = {
	  val limit = text.length
		var cursor = current
		while(cursor < limit && !until(text(cursor))) cursor += 1
		cursor
	}
	private def isDigit(c: Char) = c == '+' || c == '-' || c == '.' || Character.isDigit(c)
	private def parseNumber(text: String, current: Int): (JsonNumber, Int) = {
	  val end = skipUntil(text, current, {c => !isDigit(c)})
	  (new JsonNumber(text.substring(current, end).toDouble), end)
	}
	private def parseBoolean(text: String, current: Int): (JsonBoolean, Int) = {
	  def getChars(length: Int): String = {
		  text.substring(current, scala.math.min(text.length, current + length))
		}
		if(getChars(5) == "false") (JsonFalse, current + 5) 
		else if(getChars(4) == "true") (JsonTrue, current + 4)
		else throw new NotABooleanException(text, current)
	}
	private final val escaped= "\\\\\"".r
	private def parseString(text: String, start: Int): (JsonString, Int) = {
	  def findRightBracket(current: Int): Int = {
		  val index = text.indexOf('"', current)
			if(index < 0) throw new UnclosedStringException(text, start)
			else {
			  if(text(index - 1) != '\\') index
				else findRightBracket(index + 1)
		  }
	  }
		val right = findRightBracket(start + 1)
		val unescaped = escaped.replaceAllIn(text.substring(start + 1, right), "\"")
		(new JsonString(unescaped), right + 1 )
	}
	private def parseList(text: String, current: Int): (JsonList, Int) = {
	  def parseItems(cursor: Int): (JsonList, Int) = {
		  val charAndPos = firstNotWhiteChar(text, cursor)
			val newPos = charAndPos._2 
			charAndPos._1 match {
				case ']' => (JsonEmptyList, cursor + 1)
				case ',' => parseItems(newPos + 1)
				case _ => {
					val objAndPos = parse(text, newPos)
					val tailAndPos = parseItems(objAndPos._2)
					(objAndPos._1 :: tailAndPos._1, tailAndPos._2)
				}
			}
		}
		if(getCharAt(text, current + 1) == ']') (JsonEmptyList, current + 2)
		else parseItems(current + 1)
	}
	private def parseMap(text: String, current: Int): (JsonMap, Int) = {
	  def parseTuples(cursor: Int): (JsonMap, Int) = {
		  val charAndPos = firstNotWhiteChar(text, cursor)
			val newPos = charAndPos._2
			charAndPos._1 match {
			  case '}' => (JsonEmptyMap, newPos + 1)
				case ',' => parseTuples(newPos + 1)
				case _ => {
				  val keyAndPos = parseString(text, newPos)
					val maybeColon = firstNotWhiteChar(text, keyAndPos._2)
					if(maybeColon._1 != ':') throw new ColonNotFoundException(text, maybeColon._2)
					val valueAndPos = parse(text, maybeColon._2 + 1)
					val tailAndPos = parseTuples(valueAndPos._2)
					(tailAndPos._1 + (keyAndPos._1 -> valueAndPos._1), tailAndPos._2)
				}
			}
		}
	  if(getCharAt(text, current + 1) == '}') (JsonEmptyMap, current + 2)
		else parseTuples(current + 1)
	}
}
