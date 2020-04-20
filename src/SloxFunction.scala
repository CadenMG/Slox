import java.util

class SloxFunction(val declaration: Stmt.Function, val closure: Environment) extends SloxCallable {
  override def arity(): Int = {
    declaration.params.size()
  }

  override def call(interpreter: Interpreter, arguments: util.ArrayList[Any]): Any = {
    val environment = new Environment(closure)
    for (i <- 0 until declaration.params.size()) {
      environment.define(declaration.params.get(i).lexeme, arguments.get(i))
    }
    try {
      interpreter.executeBlock(declaration.body, environment)
    }
    catch {
      case e: Return => e.value
    }
  }

  override def toString: String = {
    "<fn " + declaration.name.lexeme + ">"
  }
}
