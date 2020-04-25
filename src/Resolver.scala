import java.util

class Resolver(final val interpreter: Interpreter) extends Expr.Visitor[Unit] with Stmt.Visitor[Unit] {
  private final val scopes = new util.Stack[util.Map[String, java.lang.Boolean]]
  private var currentFunction = FunctionType.NONE
  private var currentClass = ClassType.NONE

  object FunctionType extends Enumeration {
    val NONE, FUNCTION, INITIALIZER, METHOD = Value
  }

  object ClassType extends Enumeration {
    val NONE, CLASS = Value
  }

  override def visitAssignExpr(expr: Expr.Assign): Unit = {
    resolve(expr.value)
    resolveLocal(expr, expr.name)
  }

  override def visitBinaryExpr(expr: Expr.Binary): Unit = {
    resolve(expr.left)
    resolve(expr.right)
  }

  override def visitCallExpr(expr: Expr.Call): Unit = {
    resolve(expr.callee)

    expr.arguments.forEach(resolve(_))
  }

  override def visitGetExpr(expr: Expr.Get): Unit = {
    resolve(expr.`object`)
  }

  override def visitGroupingExpr(expr: Expr.Grouping): Unit = {
    resolve(expr.expression)
  }

  override def visitLiteralExpr(expr: Expr.Literal): Unit = {

  }

  override def visitLogicalExpr(expr: Expr.Logical): Unit = {
    resolve(expr.right)
  }

  override def visitSetExpr(expr: Expr.Set): Unit = {
    resolve(expr.value)
    resolve(expr.`object`)
  }

  override def visitSuperExpr(expr: Expr.Super): Unit = ???

  override def visitThisExpr(expr: Expr.This): Unit = {
    if (this.currentClass == ClassType.NONE) {
      Slox.error(expr.keyword, "Cannot use 'this' outside of a class.")
    }
    else {
      resolveLocal(expr, expr.keyword)
    }
  }

  override def visitUnaryExpr(expr: Expr.Unary): Unit = {
    resolve(expr.right)
  }

  override def visitVariableExpr(expr: Expr.Variable): Unit = {
    if (!this.scopes.isEmpty &&
        scopes.peek().get(expr.name.lexeme) == java.lang.Boolean.FALSE) {
      Slox.error(expr.name, "Cannot read local variable in its own initializer.")
    }

    resolveLocal(expr, expr.name)
  }
  override def visitBlockStmt(stmt: Stmt.Block): Unit = {
    beginScope()
    resolve(stmt.statements)
    endScope()
  }

  override def visitClassStmt(stmt: Stmt.Class): Unit = {
    val enclosingClass = currentClass
    currentClass = ClassType.CLASS

    declare(stmt.name)
    define(stmt.name)

    beginScope()
    scopes.peek().put("this", true)

    stmt.methods.forEach(method => {
      var declaration = FunctionType.METHOD
      if (method.name.lexeme.eq("init")) {
        declaration = FunctionType.INITIALIZER
      }
      resolveFunction(method, declaration)
    })

    endScope()

    currentClass = enclosingClass
  }

  override def visitExpressionStmt(stmt: Stmt.Expression): Unit = {
    resolve(stmt.expression)
  }

  override def visitFunctionStmt(stmt: Stmt.Function): Unit = {
    declare(stmt.name)
    define(stmt.name)

    resolveFunction(stmt, FunctionType.FUNCTION)
  }

  override def visitIfStmt(stmt: Stmt.If): Unit = {
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    if (stmt.elseBranch != null) resolve(stmt.elseBranch)
  }

  override def visitPrintStmt(stmt: Stmt.Print): Unit = {
    resolve(stmt.expression)
  }

  override def visitReturnStmt(stmt: Stmt.Return): Unit = {
    if (currentFunction == FunctionType.NONE) {
      Slox.error(stmt.keyword, "Cannot return from top-level code.")
    }

    if (stmt.value != null) {
      if (currentFunction == FunctionType.INITIALIZER) {
        Slox.error(stmt.keyword, "Cannot return a value from an initializer.")
      }
      resolve(stmt.value)
    }
  }

  override def visitVarStmt(stmt: Stmt.Var): Unit = {
    declare(stmt.name)
    if (stmt.initializer != null) {
      resolve(stmt.initializer)
    }
    define(stmt.name)
  }

  override def visitWhileStmt(stmt: Stmt.While): Unit = {
    resolve(stmt.condition)
    resolve(stmt.body)
  }

  def resolve(stmts: util.ArrayList[Stmt]): Unit = {
    stmts.forEach(resolve(_))
  }

  def resolve(expr: Expr): Unit = {
    expr.accept(this)
  }

  private def resolve(stmt: Stmt): Unit = {
    stmt.accept(this)
  }

  private def resolveFunction(function: Stmt.Function, fType: FunctionType.Value): Unit = {
    val enclosingFunction = currentFunction
    currentFunction = fType

    beginScope()
    function.params.forEach { token =>
      declare(token)
      define(token)
    }
    resolve(function.body)
    endScope()
    currentFunction = enclosingFunction
  }

  private def resolveLocal(expr: Expr, name: Token): Unit = {
    for (i <- this.scopes.size() - 1 to 0 by -1) {
      if (this.scopes.get(i).containsKey(name.lexeme)) {
        this.interpreter.resolve(expr, scopes.size() - 1 - i)
      }
    }
  }

  private def declare(token: Token): Unit = {
    if (this.scopes.isEmpty) return
    val scope = scopes.peek()
    if (scope.containsKey(token.lexeme)) {
      Slox.error(token,
        "Variable with this name already declared in this scope.")
    }
    scope.put(token.lexeme, false)
  }

  private def define(token: Token): Unit = {
    if (this.scopes.isEmpty) return
    this.scopes.peek().put(token.lexeme, true)
  }

  private def beginScope(): Unit = {
    this.scopes.push(new util.HashMap[String, java.lang.Boolean]())
  }

  private def endScope(): Unit = {
    this.scopes.pop()
  }
}
