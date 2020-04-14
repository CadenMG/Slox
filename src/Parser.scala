import java.util

/**
 * Parses tokens into expression objects; which are the languages AST.
 * @param tokens  list of token objects
 */
class Parser(tokens: util.List[Token]) {
  private class ParseError extends RuntimeException {}

  /*
  program        → declaration* EOF ;
  declaration    → varDecl
                 | statement ;
  varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
  statement      → exprStmt
                 | printStmt
                 | block ;
  block          → "{" declaration* "}" ;
  exprStmt       → expression ";" ;
  printStmt      → "print" expression ";" ;
  expression     → assignment ;
  assignment     → IDENTIFIER "=" assignment
                 | equality ;
  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
  comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
  addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
  multiplication → unary ( ( "/" | "*" ) unary )* ;
  unary          → ( "!" | "-" ) unary
                 | primary ;
  primary        → NUMBER | STRING | "false" | "true" | "nil"
                 | "(" expression ")" | IDENTIFIER ;
   */

  private var current = 0

  def parse(): util.ArrayList[Stmt] = {
    val statements = new util.ArrayList[Stmt]()
    while (!isAtEnd) {
      statements.add(declaration())
    }
    statements
  }

  private def declaration(): Stmt = {
    try {
      if (`match`(TokenType.VAR)) return varDeclaration()
      statement()
    }
    catch {
      case error: ParseError =>
        synchronize()
        null
    }
  }

  private def varDeclaration(): Stmt = {
    val name = consume(TokenType.IDENTIFIER, "Expect variable name.")

    var initializer: Option[Expr] = None
    if (`match`(TokenType.EQUAL)) {
      initializer = Some(expression())
    }

    val value = initializer.orNull

    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
    new Stmt.Var(name, value)
  }

  private def statement(): Stmt = {
    if (`match`(TokenType.PRINT)) return printStatement()
    if (`match`(TokenType.LEFT_BRACE)) return new Stmt.Block(block())

    expressionStatement()
  }

  private def printStatement(): Stmt = {
    val value = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after value.")
    new Stmt.Print(value)
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after expression.")
    new Stmt.Expression(expr)
  }

  private def block(): util.ArrayList[Stmt] = {
    val statements = new util.ArrayList[Stmt]()

    while (!check(TokenType.RIGHT_BRACE) && !isAtEnd) {
      statements.add(declaration())
    }

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block")
    statements
  }

  private def expression(): Expr = {
    assignment()
  }

  private def assignment(): Expr = {
    val expr = equality()

    if (`match`(TokenType.EQUAL)) {
      val equals = previous()
      val value = assignment()

      if (expr.isInstanceOf[Expr.Variable]) {
        val name = expr.asInstanceOf[Expr.Variable].name
        return new Expr.Assign(name, value)
      }

      error(equals, "Invalid assignment target.")
    }

    expr
  }

  private def equality(): Expr = {
    var expr = comparison()

    while ( {
      `match`(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)
    }) {
      val operator = previous()
      val right = comparison()
      expr = new Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def comparison(): Expr = {
    var expr = addition()

    while ( {
      `match`(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)
    }) {
      val operator = previous()
      val right = addition()
      expr = new Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def addition(): Expr = {
    var expr = multiplication()

    while ( {
      `match`(TokenType.MINUS, TokenType.PLUS)
    }) {
      val operator = previous()
      val right = multiplication()
      expr = new Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def multiplication(): Expr = {
    var expr = unary()

    while ( {
      `match`(TokenType.SLASH, TokenType.STAR)
    }) {
      val operator = previous()
      val right = unary()
      expr = new Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def unary(): Expr = {
    if (`match`(TokenType.BANG, TokenType.MINUS)) {
      val operator = previous()
      val right = unary()
      new Expr.Unary(operator, right)
    }

    primary()
  }

  private def primary(): Expr = {
    if (`match`(TokenType.FALSE)) return new Expr.Literal(false)
    if (`match`(TokenType.TRUE)) return new Expr.Literal(true)
    if (`match`(TokenType.NIL)) return new Expr.Literal(null)

    if (`match`(TokenType.NUMBER, TokenType.STRING))
      return new Expr.Literal(previous().literal)

    if (`match`(TokenType.IDENTIFIER))
      return new Expr.Variable(previous())

    if (`match`(TokenType.LEFT_PAREN)) {
      val expr = expression()
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
      return new Expr.Grouping(expr)
    }

    throw error(peek(), "Expect expression.")
  }

  private def `match`(types: TokenType.Value*): Boolean = {
    for (tType <- types) {
      if (check(tType)) {
        advance()
        return true
      }
    }
    false
  }

  private def consume(tType: TokenType.Value, message: String): Token = {
    if (check(tType)) return advance()
    throw error(peek(), message)
  }

  private def error(token: Token, message: String): ParseError = {
    Slox.error(token, message)
    new ParseError()
  }

  private def synchronize(): Unit = {
    advance()

    while ( {
      !isAtEnd
    }) {
      if (previous().tType == TokenType.SEMICOLON) return
      peek().tType match {
        case TokenType.CLASS =>
        case TokenType.FUN =>
        case TokenType.VAR =>
        case TokenType.FOR =>
        case TokenType.IF =>
        case TokenType.WHILE =>
        case TokenType.PRINT =>
        case TokenType.RETURN =>
          return
      }
      advance()
    }
  }

  private def check(tType: TokenType.Value): Boolean = {
    if (isAtEnd) return false
    peek().tType == tType
  }

  private def advance(): Token = {
    if (!isAtEnd) this.current += 1
    previous()
  }

  private def isAtEnd: Boolean = {
    peek().tType == TokenType.EOF
  }

  private def peek(): Token = {
    this.tokens.get(this.current)
  }

  private def previous(): Token = {
    this.tokens.get(this.current - 1)
  }

}
