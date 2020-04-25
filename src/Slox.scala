import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.io.BufferedReader
import java.io.IOException

/**
 * Main driver of the Slox language.
 */
object Slox {

  private val interpreter = new Interpreter()
  var hadError = false
  var hadRuntimeError = false

  @throws[IOException]
  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      System.out.println("Usage: jlox [script]")
      System.exit(64)
    }
    else if (args.length == 1) runFile(args(0))
    else runPrompt()
  }

  @throws[IOException]
  def runFile(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset))

    if (hadError) System.exit(65)
    if (hadRuntimeError) System.exit(70)
  }

  @throws[IOException]
  private def runPrompt(): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)


    while ( {
      true
    }) {
      print("> ")
      run(reader.readLine)
      hadError = false
    }
  }

  private def run(source: String): Unit = {
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens()
    val parser = new Parser(tokens)
    val statements = parser.parse()
    if (hadError) return

    val resolver = new Resolver(interpreter)
    resolver.resolve(statements)
    if (hadError) return

    this.interpreter.interpret(statements)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line: Int, where: String, message: String): Unit = {
    println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
  }

  def error(token: Token, message: String): Unit = {
    if (token.tType == TokenType.EOF) {
      report(token.line, " at end", message)
    }
    else {
      report(token.line, " at '" + token.lexeme + "'", message)
    }
  }

  def runtimeError(error: RuntimeError): Unit = {
    println(error.getMessage + "\n[line " + error.token.line + "]")
    this.hadRuntimeError = true
  }

}
