import scala.collection.mutable

class Environment(enclosing: Environment = null) {
  private final val values = new mutable.HashMap[String, Any]()

  def get(name: Token): Any = {
    val lexeme = name.lexeme
    if (this.values.contains(lexeme))
      return this.values.get(lexeme).orNull

    if (this.enclosing != null)
      return this.enclosing.get(name)

    throw new RuntimeError(name,"Undefined variable '" + lexeme + "'.")
  }

  def define(name: String, value: Any): Unit = {
    this.values.put(name, value)
  }

  def assign(name: Token, value: Any): Unit = {
    if (this.values.contains(name.lexeme)) {
      this.values.put(name.lexeme, value)
      return
    }

    if (this.enclosing != null) {
      this.enclosing.assign(name, value)
      return
    }

    throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
  }
}
