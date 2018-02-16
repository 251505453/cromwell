package wdl.draft3.transforms.ast2wdlom

import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.validated._
import common.validation.Validation._
import common.validation.ErrorOr.ErrorOr
import wdl.draft3.parser.WdlParser.{Ast, AstNode, Terminal}
import wdl.model.draft3.elements.ExpressionElement._
import wdl.model.draft3.elements._
import wdl.draft3.transforms.ast2wdlom.EnhancedDraft3Ast._
import wom.values._

import scala.util.Try

object AstNodeToExpressionElement {
  def convert(astNode: AstNode): ErrorOr[ExpressionElement] = astNode match {

    case t: Terminal if asPrimitive.isDefinedAt((t.getTerminalStr, t.getSourceString)) => asPrimitive((t.getTerminalStr, t.getSourceString)).map(PrimitiveLiteralExpressionElement)
    case t: Terminal => s"No rule available to create ExpressionElement from terminal: ${t.getTerminalStr} ${t.getSourceString}".invalidNel

    case a: Ast if a.getName == "Add" => getLhsAndRhs(a, Add.apply)
  }

  private def getLhsAndRhs(a: Ast, combiner: (ExpressionElement, ExpressionElement) => ExpressionElement): ErrorOr[ExpressionElement] = {
    val lhsValidation: ErrorOr[ExpressionElement] = a.getAttributeAs[ExpressionElement]("lhs").toValidated
    val rhsValidation: ErrorOr[ExpressionElement] = a.getAttributeAs[ExpressionElement]("rhs").toValidated

    (lhsValidation, rhsValidation) mapN { combiner }
  }

  private val asPrimitive: PartialFunction[(String, String), ErrorOr[WomPrimitive]] = {
    case ("integer", i) => Try(WomInteger(i.toInt)).toErrorOr
    case ("float", f) => Try(WomFloat(f.toDouble)).toErrorOr
    case ("boolean", b) => Try(WomBoolean(b.toBoolean)).toErrorOr
    case ("string", s) => WomString(s).validNel
    case ("file", s) => WomString(s).validNel
  }
}
