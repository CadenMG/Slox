import java.util

import scala.collection.mutable

/**
 * Interprets expression objects which represent the language's AST.
 */
class Interpreter extends Expr.Visitor[Any] with Stmt.Visitor[Unit] {

  final var globals = new Environment()
  private var environment = globals
  private final val locals = new mutable.HashMap[Expr, Int]()

  this.globals.define("clock", new SloxCallable {
    override def arity(): Int = 0

    override def call(interpreter: Interpreter, arguments: util.ArrayList[Any]): Any = {
      System.currentTimeMillis().asInstanceOf[Double] / 10000
    }

    override def toString: String = {
      "<native fn>"
    }
  })

  def interpret(statements: util.ArrayList[Stmt]): Unit = {
    try {
      statements.forEach {
        execute(_)
      }
    }
    catch {
      case error: RuntimeError => Slox.runtimeError(error)
    }
  }

  override def visitAssignExpr(expr: Expr.Assign): Any = {
    val value = evaluate(expr.value)

    val distance = this.locals.get(expr)
    if (distance.nonEmpty) {
      this.environment.assignAt(distance.get, expr.name, value)
    }
    else {
      this.globals.assign(expr.name, value)
    }

    value
  }

  override def visitBinaryExpr(expr: Expr.Binary): Any = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tType match {
      case TokenType.MINUS =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] - right.asInstanceOf[Double]
      case TokenType.SLASH =>
        checkNumberOperands(expr.operator, left, right)
        if (right.asInstanceOf[Double] == 0.0) {
          throw new RuntimeError(expr.operator, "Division by zero.")
        }
        return left.asInstanceOf[Double] / right.asInstanceOf[Double]
      case TokenType.STAR =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] * right.asInstanceOf[Double]
      case TokenType.PLUS =>
        if (left.isInstanceOf[Double] && right.isInstanceOf[Double])
          return left.asInstanceOf[Double] + right.asInstanceOf[Double]
        if ( (left.isInstanceOf[String] || right.isInstanceOf[String])
          && (left != null && right != null))
          return left.toString + right.toString
        throw new RuntimeError(expr.operator,
        "Operands must be two numbers or at least one string and not nil.")
      case TokenType.GREATER =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] > right.asInstanceOf[Double]
      case TokenType.GREATER_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] >= right.asInstanceOf[Double]
      case TokenType.LESS =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] < right.asInstanceOf[Double]
      case TokenType.LESS_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] <= right.asInstanceOf[Double]
      case TokenType.BANG_EQUAL =>
        return !isEqual(left, right)
      case TokenType.EQUAL_EQUAL =>
        return isEqual(left, right)
    }

    null
  }

  override def visitCallExpr(expr: Expr.Call): Any = {
    val callee = evaluate(expr.callee)
    val arguments = new util.ArrayList[Any]
    expr.arguments.forEach(arg => arguments.add(evaluate(arg)))
    val function = callee.asInstanceOf[SloxCallable]
    if (arguments.size() != function.arity()) {
      throw new RuntimeError(expr.paren, "Expected " +
        function.arity() + " arguments but got " +
        arguments.size() + ".")
    }
    function.call(this, arguments)
  }

  override def visitGetExpr(expr: Expr.Get): Any = {
    val obj = evaluate(expr.`object`)
    if (obj.isInstanceOf[SloxInstance]) {
      obj.asInstanceOf[SloxInstance].get(expr.name)
    }
  }

  override def visitGroupingExpr(expr: Expr.Grouping): Any = {
    evaluate(expr.expression)
  }

  override def visitLiteralExpr(expr: Expr.Literal): Any = {
    expr.value
  }

  override def visitLogicalExpr(expr: Expr.Logical): Any = {
    val left = evaluate(expr.left)

    if (expr.operator.tType == TokenType.OR) {
      if (isTruthy(left)) return left
    }
    else {
      if (!isTruthy(left)) return left
    }

    evaluate(expr.right)
  }

  override def visitSetExpr(expr: Expr.Set): Any = {
    val obj = evaluate(expr.`object`)

    if (! obj.isInstanceOf[SloxInstance]) {
      throw new RuntimeError(expr.name, "Only instances have fields")
    }

    val value = evaluate(expr.value)
    obj.asInstanceOf[SloxInstance].set(expr.name, value)
    value
  }

  override def visitSuperExpr(expr: Expr.Super): Any = ???

  override def visitThisExpr(expr: Expr.This): Any = {
    lookUpVariable(expr.keyword, expr)
  }

  override def visitUnaryExpr(expr: Expr.Unary): Any = {
    val right = evaluate(expr.right)

    expr.operator.tType match {
      case TokenType.BANG =>
        return !isTruthy(right)
      case TokenType.MINUS =>
        checkNumberOperand(expr.operator, right)
        return -right.asInstanceOf[Double]
    }

    null
  }

  override def visitVariableExpr(expr: Expr.Variable): Any = {
    lookUpVariable(expr.name, expr)
  }

  private def lookUpVariable(name: Token, expr: Expr): Any = {
    val distance = this.locals.get(expr)

    if (distance.nonEmpty) {
      environment.getAt(distance.get, name.lexeme)
    }
    else {
      globals.get(name)
    }
  }

  private def evaluate(expr: Expr): Any = {
    expr.accept(this)
  }

  private def execute(stmt: Stmt): Unit = {
    stmt.accept(this)
  }

  private def isTruthy(any: Any): Boolean = {
    if (any == null) return false
    if (any.isInstanceOf[Boolean]) return any.asInstanceOf[Boolean]
    true
  }

  private def isEqual(value1: Any, value2: Any): Boolean = {
    if (value1 == null && value2 == null) return true
    if (value1 == null) return false
    value1.equals(value2)
  }

  private def checkNumberOperand(token: Token, value: Any): Unit = {
    if (value.isInstanceOf[Double]) return
    throw new RuntimeError(token, "Operand must be a number.")
  }

  private def checkNumberOperands(token: Token, value1: Any, value2: Any): Unit = {
    if (value1.isInstanceOf[Double] && value2.isInstanceOf[Double]) return
    throw new RuntimeError(token, "Operands must be numbers.")
  }

  private def stringify(any: Any): String = {
    if (any == null) return "nil"
    if (any.isInstanceOf[Double]) {
      var text = any.toString
      if (text.endsWith(".0")) text = text.substring(0, text.length - 2)
      return text
    }
    any.toString
  }

  def executeBlock(stmts: util.ArrayList[Stmt], environment: Environment): Unit = {
    val previous = this.environment

    try {
      this.environment = environment
      stmts.forEach(execute(_))
    }
    finally {
      this.environment = previous
    }
  }

  override def visitBlockStmt(stmt: Stmt.Block): Unit = {
    executeBlock(stmt.statements, new Environment(this.environment))
  }

  override def visitClassStmt(stmt: Stmt.Class): Unit = {
    this.environment.define(stmt.name.lexeme, null)

    val methods = new mutable.HashMap[String, SloxFunction]()
    stmt.methods.forEach(method => methods.put(method.name.lexeme,
      new SloxFunction(method, environment, method.name.lexeme.eq("init"))))

    val klass = new SloxClass(stmt.name.lexeme, methods)
    environment.assign(stmt.name, klass)
  }

  override def visitExpressionStmt(stmt: Stmt.Expression): Unit = {
    evaluate(stmt.expression)
  }

  override def visitFunctionStmt(stmt: Stmt.Function): Unit = {
    val function = new SloxFunction(stmt, environment, false)
    environment.define(stmt.name.lexeme, function)
  }

  override def visitIfStmt(stmt: Stmt.If): Unit = {
    if (isTruthy(evaluate(stmt.condition))) {
      execute(stmt.thenBranch)
    }
    else if (stmt.elseBranch != null) {
      execute(stmt.elseBranch)
    }
  }

  override def visitPrintStmt(stmt: Stmt.Print): Unit = {
    val value = evaluate(stmt.expression)
    println(stringify(value))
  }

  override def visitReturnStmt(stmt: Stmt.Return): Unit = {
    var value: Option[Any] = None
    if (stmt.value != null) value = Some(evaluate(stmt.value))

    throw new Return(value.orNull)
  }

  override def visitVarStmt(stmt: Stmt.Var): Unit = {
    var value: Option[Any] = None
    if (stmt.initializer != null) {
      val t = evaluate(stmt.initializer)
      value = Some(t)
    }
    val equals = value.orNull

    this.environment.define(stmt.name.lexeme, equals)
  }

  override def visitWhileStmt(stmt: Stmt.While): Unit = {
    while (isTruthy(evaluate(stmt.condition))) {
      execute(stmt.body)
    }
  }

  def resolve(expr: Expr, depth: Int): Unit = {
    this.locals.put(expr, depth)
  }
}
