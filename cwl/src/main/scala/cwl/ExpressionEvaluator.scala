package cwl

import cats.syntax.validated._
import common.validation.ErrorOr._
import common.validation.Validation._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import shapeless.Witness
import wom.callable.RuntimeEnvironment
import wom.types.WomStringType
import wom.util.JsUtil
import wom.values.{WomFloat, WomInteger, WomString, WomValue}

// http://www.commonwl.org/v1.0/CommandLineTool.html#Expressions
object ExpressionEvaluator {
  // A code fragment wrapped in the $(...) syntax must be evaluated as a ECMAScript expression.
  val ECMAScriptExpressionWitness = Witness("DONT_MATCH_AS_INTERPOLATED_STRING_WORKS_FINE_BUT_COMES_AFTER_IN_COPRODUCT")
  val ECMAScriptExpressionRegex = ECMAScriptExpressionWitness.value.r
  type MatchesECMAScript = MatchesRegex[ECMAScriptExpressionWitness.T]
  type ECMAScriptExpression = String Refined MatchesECMAScript

  // A code fragment wrapped in the ${...} syntax must be evaluated as a ECMAScript function body for an anonymous,
  // zero-argument function.
  val ECMAScriptFunctionWitness = Witness("""(?s)\s*\$\{(.*)\}\s*""")
  val ECMAScriptFunctionRegex = ECMAScriptFunctionWitness.value.r
  type MatchesECMAFunction = MatchesRegex[ECMAScriptFunctionWitness.T]
  type ECMAScriptFunction = String Refined MatchesECMAFunction

  /*
  Via:
  - https://stackoverflow.com/questions/47162098#answer-47162099
  Online tests:
  - https://regex101.com/
  - http://java-regex-tester.appspot.com/

  Returns matches that begin with "$(" and end with ")" ensuring that parens are balanced within.
   */
  val InterpolatedStringCore =
    """(?=\$\()(?:(?=.*?\((?!.*?\1)(.*\)(?!.*\2).*))(?=.*?\)(?!.*?\2)(.*)).)+?.*?(?=\1)[^(]*(?=\2$)"""

  /*
  The pattern for an interpolated string, with perhaps some leading or trailing characters.
  Not sure how to use this with `replaceAllIn()`, so there is a separate InterpolatedStringRegex.
   */
  val InterpolatedStringWitnessPattern = s"(?s).*?$InterpolatedStringCore.*?"
  val InterpolatedStringRegex = s"(?s)$InterpolatedStringCore".r

  // Inner regex. Should actually just be stripping "$(", ")", but JIC allowing whitespace too.
  val InterpolatedExpressionRegex =
    """(?s)\s*\$\((.*)\)\s*""".r

  // This is not ECMAScript, just what CWL uses for interpolated expressions. The pattern is 'before$(expression)after'.
  // The regex uses a non-greedy quantifier on the 'before' to allow the expression to be processed from left to right,
  // and there are capturing groups around each portion. The official specification for interpolated strings is in the
  // last part of this section:
  // http://www.commonwl.org/v1.0/CommandLineTool.html#Parameter_references
  val InterpolatedStringWitness = Witness(InterpolatedStringWitnessPattern)
  type MatchesInterpolatedString = MatchesRegex[InterpolatedStringWitness.T]
  type InterpolatedString = String Refined MatchesInterpolatedString

  def evalExpression(expression: ECMAScriptExpression, parameterContext: ParameterContext, expressionLib: ExpressionLib): ErrorOr[WomValue] = {
    expression.value match {
      case ECMAScriptExpressionRegex(script) =>
        // Nashorn doesn't like an expression floating around. So assign it to a variable and return that variable.
        val variableExpression =
          s"""|var expression_result = EXPRESSION_BODY;
              |expression_result
              |""".stripMargin.replace("EXPRESSION_BODY", script)
        
        eval(expressionFromParts(expressionLib, variableExpression), parameterContext)
      case unmatched =>
        s"Expression '$unmatched' was unable to be matched to regex '${ECMAScriptExpressionWitness.value}'".invalidNel
    }
  }

  def expressionFromParts(lib: ExpressionLib, script: String) = {
    val expressionScript: String = lib.mkString(";")
    s"$expressionScript;$script"
  }

  def evalFunction(function: ECMAScriptFunction, parameterContext: ParameterContext, expressionLib: ExpressionLib): ErrorOr[WomValue] = {
    function.value match {
      case ECMAScriptFunctionRegex(script) =>

        val functionExpression =
          s"""|(function() {
              |FUNCTION_BODY
              |})();
              |""".stripMargin.replace("FUNCTION_BODY", script)

        eval(expressionFromParts(expressionLib, functionExpression), parameterContext)
      case unmatched =>
        s"Expression '$unmatched' was unable to be matched to regex '${ECMAScriptFunctionWitness.value}'".invalidNel
    }
  }

  def evalInterpolatedString(string: InterpolatedString,
                             parameterContext: ParameterContext,
                             expressionLib: ExpressionLib): ErrorOr[WomString] = {
    validate {
      InterpolatedStringRegex.replaceAllIn(string.value, { matchData =>
        // The match will start with "$(" and end with ")". Get the content within.
        val expression = matchData.matched match {
          case InterpolatedExpressionRegex(content) => content
        }
        val evaled = eval(expressionFromParts(expressionLib, expression), parameterContext)
        evaled.toTry(s"evaluating: $expression").get.valueString
      })
    } map WomString
  }

  private lazy val cwlJsEncoder = new CwlJsEncoder()
  private lazy val cwlJsDecoder = new CwlJsDecoder()

  def eval(expr: String, parameterContext: ParameterContext): ErrorOr[WomValue] = {
    val (rawValues, mapValues) = paramValues(parameterContext)
    JsUtil.evalStructish(expr, rawValues, mapValues, cwlJsEncoder, cwlJsDecoder)
  }

  def eval(expr: Expression, parameterContext: ParameterContext, expressionLib: ExpressionLib): ErrorOr[WomValue] = {
    expr.fold(EvaluateExpression).apply(parameterContext, expressionLib)
  }

  def paramValues(parameterContext: ParameterContext): (Map[String, WomValue], Map[String, Map[String, WomValue]]) = {
    (
      Map(
        "self" -> parameterContext.self
      ),
      Map(
        "inputs" -> parameterContext.inputs,
        "runtime" -> parameterContext.runtimeOption.map(cwlMap).getOrElse(Map.empty)
      )
    )
  }

  def cwlMap(runtime: RuntimeEnvironment): Map[String, WomValue] = {
    Map(
      "outdir" -> WomString(runtime.outputPath),
      "tmpdir" -> WomString(runtime.tempPath),
      "cores" -> WomInteger(runtime.cores),
      "ram" -> WomFloat(runtime.ram),
      "outdirSize" -> WomFloat(runtime.outputPathSize.toDouble),
      "tmpdirSize" -> WomFloat(runtime.tempPathSize.toDouble)
    )
  }
}
