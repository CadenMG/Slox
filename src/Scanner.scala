import java.util

/**
 * The main scanner class for the Slox language.
 * @param source  input source code
 */
class Scanner(val source: String) {

  final private val tokens = new util.ArrayList[Token]
  final private val keywords = setupKeywords()
  private var start = 0
  private var current = 0
  private var line = 1
  private val ESCAPE = '\u0000'

  def scanTokens() : util.List[Token] = {
    while (!isAtEnd) {
      this.start = current
      scanToken()
    }
    tokens.add(new Token(TokenType.EOF, "", null, line))
    tokens
  }

  private def isAtEnd: Boolean = {
    current >= source.length()
  }

  private def scanToken(): Unit = {
    val c = advance
    c match {
      case '(' =>
        addToken(TokenType.LEFT_PAREN)

      case ')' =>
        addToken(TokenType.RIGHT_PAREN)

      case '{' =>
        addToken(TokenType.LEFT_BRACE)

      case '}' =>
        addToken(TokenType.RIGHT_BRACE)

      case ',' =>
        addToken(TokenType.COMMA)

      case '.' =>
        addToken(TokenType.DOT)

      case '-' =>
        addToken(TokenType.MINUS)

      case '+' =>
        addToken(TokenType.PLUS)

      case ';' =>
        addToken(TokenType.SEMICOLON)

      case '*' =>
        addToken(TokenType.STAR)

      case '!' =>
        addToken(if (`match`('=')) TokenType.BANG_EQUAL else TokenType.BANG)

      case '=' =>
        addToken(if (`match`('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL)

      case '<' =>
        addToken(if (`match`('=')) TokenType.LESS_EQUAL else TokenType.LESS)

      case '>' =>
        addToken(if (`match`('=')) TokenType.GREATER_EQUAL else TokenType.GREATER)

      case '/' =>
        if (`match`('/')) { // A comment goes until the end of the line.
          while ( {
            peek != '\n' && !isAtEnd
          }) advance
        }
        else addToken(TokenType.SLASH)

      case ' ' =>

      case '\r' =>

      case '\t' =>

      case '\n' =>
        line += 1

      case '"' =>
        string()

      case _ =>
        if (isDigit(c))
          number()
        else if (isAlpha(c))
          identifier()
        else
          Slox.error(line, "Unexpected character.")
    }
  }

  private def number(): Unit = {
    while ( {
      isDigit(peek)
    }) advance

    // Look for a fractional part.
    if ((peek == '.') && isDigit(peekNext)) { // Consume the "."
      advance
      while ( {
        isDigit(peek)
      }) advance
    }

    addToken(TokenType.NUMBER, source.substring(start, current).toDouble)
  }

  private def string(): Unit = {
    while ( {
      (peek != '"') && !isAtEnd
    }) {
      if (peek == '\n') line += 1
      advance
    }

    // Unterminated string.
    if (isAtEnd) {
      Slox.error(line, "Unterminated string.")
      return
    }

    // The closing ".
    advance

    // Trim the surrounding quotes.
    val value = source.substring(start + 1, current - 1)
    addToken(TokenType.STRING, value)
  }

  private def identifier(): Unit = {
    while ( {
      isAlphaNumeric(peek)
    }) {
      advance
    }

    // See if the identifier is a reserved word.
    val text = this.source.substring(this.start, this.current)

    var tType = this.keywords.get(text)

    // if not a reserved word then set to an identifier
    if (tType == null) tType = TokenType.IDENTIFIER
    addToken(tType)
  }

  private def isDigit(c: Char): Boolean = {
    c >= '0' && c <= '9'
  }

  private def isAlpha(c: Char): Boolean = {
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
     c == '_'
  }

  private def isAlphaNumeric(c: Char): Boolean = {
    isAlpha(c) || isDigit(c)
  }

  private def peek: Char = {
    if (isAtEnd) return ESCAPE;
    source.charAt(current);
  }

  private def peekNext: Char = {
    if (this.current + 1 > this.source.length) return ESCAPE
    source.charAt(this.current + 1)
  }

  private def `match` (expected: Char): Boolean = {
    if (isAtEnd) return false
    if (source.charAt(this.current) != expected) return false

    this.current += 1
    true
  }

  private def advance: Char = {
    this.current += 1
    source.charAt(this.current - 1)
  }

  private def addToken(tType: TokenType.Value): Unit = {
    addToken(tType, null)
  }

  private def addToken(tType: TokenType.Value, literal: Any): Unit = {
    val text = source.substring(this.start, this.current)
    tokens.add(new Token(tType, text, literal, this.line))
  }

  private def setupKeywords(): util.HashMap[String, TokenType.Value] = {
    val keys = new util.HashMap[String, TokenType.Value]
    keys.put("and",    TokenType.AND)
    keys.put("class",  TokenType.CLASS)
    keys.put("else",   TokenType.ELSE)
    keys.put("false",  TokenType.FALSE)
    keys.put("for",    TokenType.FOR)
    keys.put("fun",    TokenType.FUN)
    keys.put("if",     TokenType.IF)
    keys.put("nil",    TokenType.NIL)
    keys.put("or",     TokenType.OR)
    keys.put("print",  TokenType.PRINT)
    keys.put("return", TokenType.RETURN)
    keys.put("super",  TokenType.SUPER)
    keys.put("this",   TokenType.THIS)
    keys.put("true",   TokenType.TRUE)
    keys.put("var",    TokenType.VAR)
    keys.put("while",  TokenType.WHILE)
    keys
  }

}