package cwl

import common.validation.Validation._
import cwl.ExpressionEvaluator._
import eu.timepit.refined._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import wom.callable.RuntimeEnvironment
import wom.values.{WomBoolean, WomSingleFile, WomString, WomValue}

class ExpressionEvaluatorSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "ExpressionEvaluator"

  it should "eval inputs of different types" in {
    val values: Map[String, WomValue] = Map(
      "myName" -> WomString("hi"),
      "someother" -> WomBoolean(false)
    )
    val parameterContext = ParameterContext(values)
    ExpressionEvaluator.eval("inputs.myName", parameterContext).toTry.get should be(WomString("hi"))
    ExpressionEvaluator.eval("inputs.someother", parameterContext).toTry.get should be(WomBoolean(false))
  }

  private val interpolatedStringTests = Table(
    ("description", "expression", "expected"),
    ("nested parens", """$(parseInt("6")).$(parseInt(("2")) + "" + parseInt(((("8")))))""", "6.28"),
    ("prefixed/suffixed nested parens",
      """6.$(parseInt("2"))$(parseInt(("8")) + "" + parseInt(((("3")))))1""",
      "6.2831"),
    ("expression libs",
      """$(t("The file is <%= data.inputs.file1.path.split('/').slice(-1)[0] %>\n"))""",
      "The file is my.file.txt\n"),
    ("two expressions", """$(runtime.outdir)/$(inputs.cram.basename)""", "out/my.cram"),
    ("two expressions suffixed", """$(runtime.outdir)/$(inputs.cram.basename).crai""", "out/my.cram.crai"),
    ("two expressions prefixed/suffixed",
      """/path/to/$(runtime.outdir)/$(inputs.cram.basename).crai""",
      "/path/to/out/my.cram.crai")
  )

  private lazy val interpolatedStringsExpressionLib = Vector(
    better.files.File(getClass.getResource("underscore.js").getPath).contentAsString,
    """var t = function(s) { return _.template(s, {variable: 'data'})({'inputs': inputs}); };"""
  )

  private lazy val interpolatedStringsParameterContext = {
    val runtime = RuntimeEnvironment("out", "tmp", 1, 2.0D, 100L, 200L)
    val inputs = Map(
      "cram" -> WomSingleFile("/path/to/my.cram"),
      "file1" -> WomSingleFile("/path/to/my.file.txt")
    )
    ParameterContext(inputs = inputs, runtimeOption = Option(runtime))
  }

  forAll(interpolatedStringTests) { (description, expression, expected) =>
    it should s"interpolate $description" in {
      val interpolatedString = refineV[MatchesInterpolatedString](expression).right.get
      val actual = ExpressionEvaluator.evalInterpolatedString(
        interpolatedString,
        interpolatedStringsParameterContext,
        interpolatedStringsExpressionLib
      )
      actual.toTry.get.valueString should be(expected)
    }
  }
}
