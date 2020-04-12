class ASTPrinter extends Expr.Visitor[String] {

  def print(expr: Expr): String = {
    expr.accept(this)
  }

  override def visitAssignExpr(expr: Expr.Assign): String = ???

  override def visitBinaryExpr(expr: Expr.Binary): String = {
    parenthesize(expr.operator.lexeme, expr.left, expr.right)
  }

  override def visitCallExpr(expr: Expr.Call): String = ???

  override def visitGetExpr(expr: Expr.Get): String = ???

  override def visitGroupingExpr(expr: Expr.Grouping): String = {
    parenthesize("group", expr.expression)
  }

  override def visitLiteralExpr(expr: Expr.Literal): String = {
    if (expr.value == null) return "nil"
    expr.value.toString
  }

  override def visitLogicalExpr(expr: Expr.Logical): String = ???

  override def visitSetExpr(expr: Expr.Set): String = ???

  override def visitSuperExpr(expr: Expr.Super): String = ???

  override def visitThisExpr(expr: Expr.This): String = ???

  override def visitUnaryExpr(expr: Expr.Unary): String = {
    parenthesize(expr.operator.lexeme, expr.right)
  }

  override def visitVariableExpr(expr: Expr.Variable): String = ???

  private def parenthesize(name: String, exprs: Expr*): String = {
    val builder = new StringBuilder()

    builder.append("(").append(name)
    for (expr <- exprs) {
      builder.append(" ")
      builder.append(expr.accept(this))
    }
    builder.append(")");

    builder.toString();
  }

}
