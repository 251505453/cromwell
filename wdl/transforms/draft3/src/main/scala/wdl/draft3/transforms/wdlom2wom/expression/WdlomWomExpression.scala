package wdl.draft3.transforms.wdlom2wom.expression

import cats.syntax.validated._
import common.validation.ErrorOr.ErrorOr
import wdl.draft3.transforms.linking.expression.types._
import wdl.draft3.transforms.linking.expression.values._
import wdl.model.draft3.graph.expression.TypeEvaluator.ops._
import wdl.model.draft3.graph.expression.ValueEvaluator.ops._
import wdl.model.draft3.elements.ExpressionElement
import wdl.model.draft3.elements.ExpressionElement.{IdentifierLookup, ObjectLiteral, PrimitiveLiteralExpressionElement}
import wdl.model.draft3.graph.{GeneratedValueHandle, UnlinkedConsumedValueHook}
import wom.expression.{IoFunctionSet, WomExpression}
import wom.types.WomType
import wom.values.{WomFile, WomValue}

final case class WdlomWomExpression(expressionElement: ExpressionElement, linkedValues: Map[UnlinkedConsumedValueHook, GeneratedValueHandle]) extends WomExpression {
  override def sourceString: String = expressionElement.toString

  override def inputs: Set[String] = linkedValues.map(_._2.linkableName).toSet

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = expressionElement match {
    case primitive: PrimitiveLiteralExpressionElement => primitive.evaluateValue(inputValues, linkedValues)
    case id: IdentifierLookup => id.evaluateValue(inputValues, linkedValues)
    case other => s"Unable to process ${other.getClass.getSimpleName}: No evaluateValue exists for that type.".invalidNel
    // TODO other expression elements
  }

  // NB types can be determined using the linked values, so we don't need the inputMap:
  override def evaluateType(inputMap: Map[String, WomType]): ErrorOr[WomType] = expressionElement match {
    case primitive: PrimitiveLiteralExpressionElement => primitive.evaluateType(linkedValues)
    case id: IdentifierLookup => id.evaluateType(linkedValues)
    case o: ObjectLiteral => o.evaluateType(linkedValues)
    case other => s"Unable to process ${other.getClass.getSimpleName}: No evaluateType exists for that type.".invalidNel
    // TODO other expression elements
  }

  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] = {
    Set.empty[WomFile].validNel
  }
}
