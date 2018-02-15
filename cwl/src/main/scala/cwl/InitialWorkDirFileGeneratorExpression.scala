package cwl

import common.validation.ErrorOr.ErrorOr
import common.validation.Validation.validate
import cwl.InitialWorkDirRequirement.IwdrListingArrayEntry
import wom.expression.IoFunctionSet
import wom.types.{WomArrayType, WomSingleFileType, WomStringType, WomType}
import wom.values.{WomArray, WomFile, WomSingleFile, WomString, WomValue}


import cats.syntax.validated._
import common.validation.ErrorOr.{ErrorOr, ShortCircuitingFlatMap}
import common.validation.Validation._
import cwl.InitialWorkDirRequirement.IwdrListingArrayEntry
import wom.expression.IoFunctionSet

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import io.circe.syntax._

final case class InitialWorkDirFileGeneratorExpression(entry: IwdrListingArrayEntry, expressionLib: ExpressionLib) extends CwlWomExpression {
  override def cwlExpressionType: WomType = WomSingleFileType
  override def sourceString: String = entry.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = {
    def mustBeString(womValue: WomValue): ErrorOr[String] = womValue match {
      case WomString(s) => s.validNel
      case other => WomStringType.coerceRawValue(other).map(_.asInstanceOf[WomString].value).toErrorOr
    }

    def evaluateEntryName(stringOrExpression: StringOrExpression): ErrorOr[String] = stringOrExpression match {
      case StringOrExpression.String(s) => s.validNel
      case StringOrExpression.Expression(entrynameExpression) => for {
        entryNameExpressionEvaluated <- ExpressionEvaluator.eval(entrynameExpression, ParameterContext(inputValues), expressionLib)
        entryNameValidated <- mustBeString(entryNameExpressionEvaluated)
      } yield entryNameValidated
    }

    entry match {
      case IwdrListingArrayEntry.StringDirent(content, direntEntryName, _) => for {
        entryNameValidated <- evaluateEntryName(direntEntryName)
        writtenFile <- validate(Await.result(ioFunctionSet.writeFile(entryNameValidated, content), Duration.Inf))
      } yield writtenFile

      case IwdrListingArrayEntry.ExpressionDirent(content, direntEntryName, _) =>
        val entryEvaluation: ErrorOr[WomValue] = content match {
          case Expression.ECMAScriptExpression(expr) if expr.value == "$(JSON.stringify(inputs))" =>
            val jsonValue: String = io.circe.Printer.noSpaces.pretty(inputValues.asJson)
            WomSingleFile(jsonValue).validNel
          case other =>ExpressionEvaluator.eval(other, ParameterContext(inputValues), expressionLib)
        }

        entryEvaluation flatMap {
          case f: WomFile =>
            val errorOrEntryName: ErrorOr[String] = direntEntryName match {
              case Some(en) => evaluateEntryName(en)
              case None => f.value.split('/').last.validNel
            }
            errorOrEntryName flatMap { entryName =>
              validate(Await.result(ioFunctionSet.copyFile(f.value, entryName), Duration.Inf))
            }
          case other => for {
            coerced <- WomStringType.coerceRawValue(other).toErrorOr
            contentString = coerced.asInstanceOf[WomString].value
            // We force the entryname to be specified, and then evaluate it:
            entryNameUnoptioned <- direntEntryName.toErrorOr("Invalid dirent: Entry was a string but no file name was supplied")
            entryname <- evaluateEntryName(entryNameUnoptioned)
            writtenFile <- validate(Await.result(ioFunctionSet.writeFile(entryname, contentString), Duration.Inf))
          } yield writtenFile
        }
      case IwdrListingArrayEntry.Expression(expression) =>
        // A single expression which must evaluate to an array of Files
        val expressionEvaluation = ExpressionEvaluator.eval(expression, ParameterContext(inputValues), expressionLib)

        expressionEvaluation flatMap {
          case array: WomArray if WomArrayType(WomSingleFileType).coercionDefined(array) => WomArrayType(WomSingleFileType).coerceRawValue(array).toErrorOr
          case file: WomSingleFile => file.validNel
          case other => s"InitialWorkDirRequirement listing expression must be Array[File] but got ${other.womType.toDisplayString}".invalidNel
        }

      case _ => ??? // TODO CWL and the rest....
    }
  }


  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] =
    "Programmer error: Shouldn't use InitialWorkDirRequirement listing to find output files. You silly goose.".invalidNel

  /**
    * We already get all of the task inputs when evaluating, and we don't need to highlight anything else
    */
  override def inputs: Set[String] = Set.empty
}
