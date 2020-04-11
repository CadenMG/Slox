
/**
 * A token within the Slox language.
 * @param tType   the token's type
 * @param lexeme  raw string of the token
 * @param literal Slox representation of the token
 * @param line    line the token occurs on
 */
class Token(val tType: TokenType.Value, val lexeme: String, val literal: Any, val line: Int) {
  override def toString: String = tType + " " + lexeme + " " + literal
}