import java.util

trait SloxCallable {
  def arity(): Int
  def call(interpreter: Interpreter, arguments: util.ArrayList[Any]): Any
}
