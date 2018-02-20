package wdl.model.draft3.elements

/**
  * Content of an intermediate or output declaration:
  */
final case class DeclarationContent(typeElement: TypeElement, name: String, expression: ExpressionElement)

/**
  * A Declaration outside of an input or output block
  */
final case class IntermediateValueDeclarationElement(typeElement: TypeElement, name: String, expression: ExpressionElement) extends WorkflowGraphNodeElement with TaskBodyElement

object IntermediateValueDeclarationElement {
  def fromContent(content: DeclarationContent): IntermediateValueDeclarationElement = IntermediateValueDeclarationElement(content.typeElement, content.name, content.expression)
}

/**
  * A declaration in an output block
  */
final case class OutputDeclarationElement(typeElement: TypeElement, name: String, expression: ExpressionElement) extends LanguageElement with WorkflowGraphNodeElement

object OutputDeclarationElement {
  def fromContent(content: DeclarationContent): OutputDeclarationElement = OutputDeclarationElement(content.typeElement, content.name, content.expression)
}

/**
  * A declaration in an input block
  */
final case class InputDeclarationElement(typeElement: TypeElement, name: String, expression: Option[ExpressionElement]) extends LanguageElement with WorkflowGraphNodeElement
