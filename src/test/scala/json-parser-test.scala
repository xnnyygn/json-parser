import org.specs._

class JsonParserSpecs extends Specification {
  "parse '-1.5' must be number -1.5" in {
	  JsonParser.parse("-1.5").getT[Double] must_== -1.5
	}
	"parse '1.1.0' must throw NumberFormatException" in {
	  JsonParser.parse("1.1.0").getT[Double] must throwA[NumberFormatException]
	}
	"parse 'true' must be boolean true" in {
	  JsonParser.parse("true").getT[Boolean] must beTrue
	}
	"parse 'false' must be boolean false5" in {
	  JsonParser.parse("false").getT[Boolean] must beFalse
	}
	"parse 'foo' must throw NotABooleanException (a literal starts with 'f' will be treated as boolean false)" in {
	  JsonParser.parse("foo").getT[Boolean] must throwA[NotABooleanException]
	}
	"parse '\"foo\"' must be string foo" in {
	  JsonParser.parse("\"foo\"").getT[String] must_== "foo"
	}
	"parse '\"foo\\\"bar\"' must be string foo\"bar" in {
	  JsonParser.parse("\"foo\\\"bar\"").getT[String] must_== "foo\"bar"
	}
	"parse '\"foo' must throw UnclosedStringException" in {
	  JsonParser.parse("\"foo").getT[String] must throwA[UnclosedStringException]
	}
	"parse '[]' must be empty list" in {
	  JsonParser.parse("[]").asInstanceOf[JsonList].isEmpty must beTrue
	}
	"parse '[true]', must be list with boolean true" in {
	  JsonParser.parse("[true]").getT[List[Any]] must_== List(true)
  }	
	"parse '[1, 2]' must be list with number 1 and 2" in {
	  JsonParser.parse("[1, 2]").getT[List[Any]] must_== List(1, 2)
	}
	"parse '{}' must be empty map" in {
	  JsonParser.parse("{}").asInstanceOf[JsonMap].isEmpty must beTrue
	}
	"parse '{\"a\": 1'}' must be map with key a and value number 1" in {
	  JsonParser.parse("{\"a\": 1}").getT[Map[String, Any]] must_== Map("a" -> 1)
	}
	"parse '{\"a\": 2, \"b\": true}' must be map with a => 2, b => true" in {
	  JsonParser.parse("{\"a\": 2, \"b\": true}").getT[Map[String, Any]] must_== Map("a" -> 2, "b" -> true)
	}
	"parse '{\"a\": -1.0, \"b\": [1, true], \"c\": \"foo\"}' must be a map expected" in {
	  JsonParser.parse("{\"a\": -1.0, \"b\": [1, true], \"c\": \"foo\"}").getT[Map[String, Any]] must_==
		  Map("a" -> -1.0, "b" -> List(1, true), "c" -> "foo")
	}
}
