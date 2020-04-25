import scala.collection.mutable

class SloxInstance(private val klass: SloxClass) {
  private final val fields = new mutable.HashMap[String, Any]()

  def get(name: Token): Any = {
    if (this.fields.contains(name.lexeme)) {
      return fields.get(name.lexeme).orNull
    }

    val method = klass.findMethod(name.lexeme)
    if (method != null) return method.bind(this)

    throw new RuntimeError(name, "Undefined property '" + name.lexeme + "'.")
  }

  def set(name: Token, value: Any): Unit = {
    this.fields.put(name.lexeme, value)
  }

  override def toString: String = klass.name + " instance"
}
