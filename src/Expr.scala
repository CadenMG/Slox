import java.util

/**
 * An abstract representation of code in Slox.
 */
object Expr {

  trait Visitor[R] {
    def visitAssignExpr(expr: Expr.Assign): R

    def visitBinaryExpr(expr: Expr.Binary): R

    def visitCallExpr(expr: Expr.Call): R

    def visitGetExpr(expr: Expr.Get): R

    def visitGroupingExpr(expr: Expr.Grouping): R

    def visitLiteralExpr(expr: Expr.Literal): R

    def visitLogicalExpr(expr: Expr.Logical): R

    def visitSetExpr(expr: Expr.Set): R

    def visitSuperExpr(expr: Expr.Super): R

    def visitThisExpr(expr: Expr.This): R

    def visitUnaryExpr(expr: Expr.Unary): R

    def visitVariableExpr(expr: Expr.Variable): R
  }

  // Nested Expr classes here...
  //> expr-assign
  class Assign(val name: Token, val value: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitAssignExpr(this)
  }

  //< expr-assign
  //> expr-binary
  class Binary(val left: Expr, val operator: Token, val right: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitBinaryExpr(this)
  }

  //< expr-binary
  //> expr-call
  class Call(val callee: Expr, val paren: Token, val arguments: util.ArrayList[Expr]) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitCallExpr(this)
  }

  //< expr-call
  //> expr-get
  class Get(val `object`: Expr, val name: Token) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitGetExpr(this)
  }

  //< expr-get
  //> expr-grouping
  class Grouping(val expression: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitGroupingExpr(this)
  }

  //< expr-grouping
  //> expr-literal
  class Literal(val value: Any) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitLiteralExpr(this)
  }

  //< expr-literal
  //> expr-logical
  class Logical(val left: Expr, val operator: Token, val right: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitLogicalExpr(this)
  }

  //< expr-logical
  //> expr-set
  class Set(val `object`: Expr, val name: Token, val value: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitSetExpr(this)
  }

  //< expr-set
  //> expr-super
  class Super(val keyword: Token, val method: Token) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitSuperExpr(this)
  }

  //< expr-super
  //> expr-this
  class This(val keyword: Token) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitThisExpr(this)
  }

  //< expr-this
  //> expr-unary
  class Unary(val operator: Token, val right: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitUnaryExpr(this)
  }

  //< expr-unary
  //> expr-variable
  class Variable(val name: Token) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R = visitor.visitVariableExpr(this)
  }

}

abstract class Expr { //< expr-variable
  def accept[R](visitor: Expr.Visitor[R]): R
}