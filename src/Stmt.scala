import java.util

object Stmt {

  trait Visitor[R] {
    def visitBlockStmt(stmt: Stmt.Block): R

    def visitClassStmt(stmt: Stmt.Class): R

    def visitExpressionStmt(stmt: Stmt.Expression): R

    def visitFunctionStmt(stmt: Stmt.Function): R

    def visitIfStmt(stmt: Stmt.If): R

    def visitPrintStmt(stmt: Stmt.Print): R

    def visitReturnStmt(stmt: Stmt.Return): R

    def visitVarStmt(stmt: Stmt.Var): R

    def visitWhileStmt(stmt: Stmt.While): R
  }

  // Nested Stmt classes here...
  //> stmt-block
  class Block(val statements: util.ArrayList[Stmt]) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitBlockStmt(this)
  }

  //< stmt-block
  //> stmt-class
  class Class(val name: Token, val superclass: Expr.Variable, val methods: Nothing) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitClassStmt(this)
  }

  //< stmt-class
  //> stmt-expression
  class Expression(val expression: Expr) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitExpressionStmt(this)
  }

  //< stmt-expression
  //> stmt-function
  class Function(val name: Token, val params: Nothing, val body: Nothing) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitFunctionStmt(this)
  }

  //< stmt-function
  //> stmt-if
  class If(val condition: Expr, val thenBranch: Stmt, val elseBranch: Stmt) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitIfStmt(this)
  }

  //< stmt-if
  //> stmt-print
  class Print(val expression: Expr) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitPrintStmt(this)
  }

  //< stmt-print
  //> stmt-return
  class Return(val keyword: Token, val value: Expr) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitReturnStmt(this)
  }

  //< stmt-return
  //> stmt-var
  class Var(val name: Token, val initializer: Expr) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitVarStmt(this)
  }

  //< stmt-var
  //> stmt-while
  class While(val condition: Expr, val body: Stmt) extends Stmt {
    override def accept[R](visitor: Stmt.Visitor[R]): R = visitor.visitWhileStmt(this)
  }

}

abstract class Stmt { //< stmt-while
  def accept[R](visitor: Stmt.Visitor[R]): R
}

//< Appendix II stmt