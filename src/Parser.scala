import java.util

/**
 * Parses tokens into expression objects; which are the languages AST.
 * @param tokens  list of token objects
 */
class Parser(tokens: util.List[Token]) {
  private class ParseError extends RuntimeException {}

  /*
  program        → declaration* EOF ;
  declaration    → funDecl
                 | varDecl
                 | statement ;
  varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
  statement      → exprStmt
                 | forStmt
                 | ifStmt
                 | printStmt
                 | returnStmt
                 | whileStmt
                 | block ;
  returnStmt     → "return" expression? ";" ;
  funDecl        → "fun" function ;
  function       → IDENTIFIER "(" parameters? ")" block ;
  parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
  forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                      expression? ";"
                      expression? ")" statement ;
  whileStmt      → "while" "(" expression ")" statement ;
  ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
  block          → "{" declaration* "}" ;
  exprStmt       → expression ";" ;
  printStmt      → "print" expression ";" ;
  expression     → assignment ;
  assignment     → IDENTIFIER "=" assignment
                 | logic_or ;
  logic_or       → logic_and ( "or" logic_and )* ;
  logic_and      → equality ( "and" equality )* ;
  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
  comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
  addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
  multiplication → unary ( ( "/" | "*" ) unary )* ;
  unary          → ( "!" | "-" ) unary | call ;
  call           → primary ( "(" arguments? ")" )* ;
  arguments      → expression ( "," expression )* ;
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
      if (`match`(TokenType.FUN)) return function("function")
      if (`match`(TokenType.VAR)) return varDeclaration()
      statement()
    }
    catch {
      case error: ParseError =>
        synchronize()
        null
    }
  }

  private def function(kind: String): Stmt.Function = {
    val name = consume(TokenType.IDENTIFIER, "Expect " + kind + " name.")
    consume(TokenType.LEFT_PAREN, "Expect '(' after " + kind + "name.")
    val parameters = new util.ArrayList[Token]()
    if (!check(TokenType.RIGHT_PAREN)) {
      do {
        parameters.add(consume(TokenType.IDENTIFIER, "Expect parameter name."))
      } while (`match`(TokenType.COMMA))
    }
    consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.")

    consume(TokenType.LEFT_BRACE, "Expect '{' before " + kind + " body.")
    val body = block()
    new Stmt.Function(name, parameters, body)
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
    if (`match`(TokenType.FOR)) return forStatement()
    if (`match`(TokenType.IF)) return ifStatement()
    if (`match`(TokenType.PRINT)) return printStatement()
    if (`match`(TokenType.RETURN)) return returnStatement()
    if (`match`(TokenType.WHILE)) return whileStatement()
    if (`match`(TokenType.LEFT_BRACE)) return new Stmt.Block(block())

    expressionStatement()
  }

  private  def returnStatement(): Stmt = {
    val keyword = previous()
    var value: Option[Expr] = None
    if (!check(TokenType.SEMICOLON)) {
      value = Some(expression())
    }

    consume(TokenType.SEMICOLON, "Expect ';' after return value.")
    new Stmt.Return(keyword, value.orNull)
  }

  private def forStatement(): Stmt = {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")

    var initializer: Option[Stmt] = None
    if (`match`(TokenType.SEMICOLON)) {
      initializer = None
    }
    else if (`match`(TokenType.VAR)) {
      initializer = Some(varDeclaration())
    }
    else {
      initializer = Some(expressionStatement())
    }

    var condition: Option[Expr] = None
    if (!check(TokenType.SEMICOLON)) {
      condition = Some(expression())
    }
    consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")

    var increment: Option[Expr] = None
    if (!check(TokenType.RIGHT_PAREN)) {
      increment = Some(expression())
    }
    consume(TokenType.RIGHT_PAREN, "Expect ')' after for clause.")
    var body = statement()

    if (increment.nonEmpty) {
      body = new Stmt.Block(new util.ArrayList(util.Arrays.asList(body, new Stmt.Expression(increment.orNull))))
    }

    if (condition.isEmpty) {
      condition = Some(new Expr.Literal(true))
    }
    body = new Stmt.While(condition.orNull, body)

    if (initializer.nonEmpty) {
      body = new Stmt.Block(new util.ArrayList(util.Arrays.asList(initializer.orNull, body)))
    }

    body
  }

  private def whileStatement(): Stmt = {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")
    val body = statement()

    new Stmt.While(condition, body)
  }

  private def ifStatement(): Stmt = {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")

    val thenBranch = statement()
    var elseBranch:Option[Stmt] = None
    if (`match`(TokenType.ELSE)) {
      elseBranch = Some(statement())
    }

    new Stmt.If(condition, thenBranch, elseBranch.orNull)
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
    val expr = or()

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

  private def or(): Expr = {
    var expr = and()

    while (`match`(TokenType.OR)) {
      val operator = previous()
      val right = and()
      expr = new Expr.Logical(expr, operator, right)
    }

    expr
  }

  private def and(): Expr = {
    var expr = equality()

    while (`match`(TokenType.AND)) {
      val operator = previous()
      val right = equality()
      expr = new Expr.Logical(expr, operator, right)
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

    call()
  }

  private def call(): Expr = {
    var expr = primary()
    object AllDone extends Exception { }

    try {
      while (true) {
        if (`match`(TokenType.LEFT_PAREN)) {
          expr = finishCall(expr)
        }
        else {
          throw AllDone
        }
      }
    }
    catch {
      case _: Exception =>
    }

    expr
  }

  private def finishCall(callee: Expr): Expr = {
    val arguments = new util.ArrayList[Expr]()
    if (!check(TokenType.RIGHT_PAREN)) {
      do {
        arguments.add(expression())
      } while (`match`(TokenType.COMMA))
    }

    val paren = consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")

    new Expr.Call(callee, paren, arguments)
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
