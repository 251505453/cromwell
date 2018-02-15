package cwl

import cats.syntax.validated._
import common.validation.ErrorOr.{ErrorOr, ShortCircuitingFlatMap}
import common.validation.Validation._
import cwl.InitialWorkDirRequirement.IwdrListingArrayEntry
import wom.expression.{IoFunctionSet, WomExpression}
import wom.types._
import wom.values._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait CwlWomExpression extends WomExpression {

  def cwlExpressionType: WomType

  override def evaluateType(inputTypes: Map[String, WomType]): ErrorOr[WomType] = cwlExpressionType.validNel

  def expressionLib: ExpressionLib

  def evaluate(inputs: Map[String, WomValue], parameterContext: ParameterContext, expression: Expression, expressionLib: ExpressionLib): ErrorOr[WomValue] =
    expression.
      fold(EvaluateExpression).
      apply(parameterContext, expressionLib)
}

case class ECMAScriptWomExpression(expression: Expression,
                                   override val inputs: Set[String],
                                   override val expressionLib: ExpressionLib) extends CwlWomExpression {
  val cwlExpressionType = WomAnyType

  override def sourceString = expression match {
    case Expression.ECMAScriptExpression(s) => s.value
    case Expression.ECMAScriptFunction(s) => s.value
    case Expression.InterpolatedString(s) => s.value
  }

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet) = {
    val pc = ParameterContext(inputValues)
    evaluate(inputValues, pc, expression, expressionLib)
  }

  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType) = Set.empty[WomFile].validNel
}


