/**
 * Interprets expression objects which represent the language's AST.
 */
class Interpreter extends Expr.Visitor[Any] {

  def interpret(expr: Expr): Unit = {
    try {
      val value = evaluate(expr)
      println(stringify(value))
    }
    catch {
      case error: RuntimeError => Slox.runtimeError(error)
    }
  }

  override def visitAssignExpr(expr: Expr.Assign): Any = ???

  override def visitBinaryExpr(expr: Expr.Binary): Any = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tType match {
      case TokenType.MINUS =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] - right.asInstanceOf[Double]
      case TokenType.SLASH =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] / right.asInstanceOf[Double]
      case TokenType.STAR =>
        checkNumberOperands(expr.operator, left, right)
        return left.asInstanceOf[Double] * right.asInstanceOf[Double]
      case TokenType.PLUS =>
        if (left.isInstanceOf[Double] && right.isInstanceOf[Double])
          return left.asInstanceOf[Double] + right.asInstanceOf[Double]
        if (left.isInstanceOf[String] && right.isInstanceOf[String])
          return left.asInstanceOf[String] + right.asInstanceOf[String]
        throw new RuntimeError(expr.operator,
        "Operands must be two numbers or two strings.")
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

  override def visitCallExpr(expr: Expr.Call): Any = ???

  override def visitGetExpr(expr: Expr.Get): Any = ???

  override def visitGroupingExpr(expr: Expr.Grouping): Any = {
    evaluate(expr.expression)
  }

  override def visitLiteralExpr(expr: Expr.Literal): Any = {
    expr.value
  }

  override def visitLogicalExpr(expr: Expr.Logical): Any = ???

  override def visitSetExpr(expr: Expr.Set): Any = ???

  override def visitSuperExpr(expr: Expr.Super): Any = ???

  override def visitThisExpr(expr: Expr.This): Any = ???

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

  override def visitVariableExpr(expr: Expr.Variable): Any = ???

  private def evaluate(expr: Expr): Any = {
    expr.accept(this)
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
}
