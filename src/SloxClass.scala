import java.util

import scala.collection.mutable
class SloxClass(final val name: String, final val superclass: SloxClass,
                final val methods: mutable.Map[String, SloxFunction]) extends SloxCallable {

  def findMethod(name: String): SloxFunction = {
    if (methods.contains(name)) {
      return methods.get(name).orNull
    }

    if (superclass != null) {
      return superclass.findMethod(name)
    }

    null
  }

  override def toString: String = this.name

  override def arity(): Int = {
    val initializer = findMethod("init")
    if (initializer == null) return 0
    initializer.arity()
  }

  override def call(interpreter: Interpreter, arguments: util.ArrayList[Any]): Any = {
    val instance = new SloxInstance(this)
    val initializer = findMethod("init")
    if (initializer != null) {
      initializer.bind(instance).call(interpreter, arguments)
    }
    instance
  }
}
