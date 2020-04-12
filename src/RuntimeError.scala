/**
 * Used to represent a runtime error in Slox.
 * @param token   token causing the runtime error
 * @param message relevant information about the error
 */
class RuntimeError(val token: Token, val message: String) extends RuntimeException(message) {

}