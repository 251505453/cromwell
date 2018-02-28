package wdl.draft3.transforms.wdlom2wom

import cats.syntax.validated._
import common.validation.ErrorOr._
import common.validation.ErrorOr.ErrorOr
import wdl.draft3.transforms.wdlom2wom.graph.{GraphNodeMakerInputs, WorkflowGraphElementToGraphNode}
import wdl.model.draft3.elements.{WorkflowDefinitionElement, WorkflowGraphElement}
import wdl.draft3.transforms.linking.graph._
import wdl.model.draft3.graph.{LinkedGraph, LinkedGraphEdge}
import wom.callable.WorkflowDefinition
import wom.graph.GraphNodePort.OutputPort
import wom.graph.{GraphNode, Graph => WomGraph}
import wom.types.WomType

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

object WorkflowDefinitionElementToWomWorkflowDefinition {

  final case class WorkflowDefinitionConvertInputs(definitionElement: WorkflowDefinitionElement, typeAliases: Map[String, WomType])

  def convert(a: WorkflowDefinitionConvertInputs): ErrorOr[WorkflowDefinition] = {

    // Make the set of workflow graph elements, including:
    // - Top-level graph elements
    // - Declarations in the inputs section
    // - Declarations in the outputs section
    val graphNodeElements: Set[WorkflowGraphElement] =
      a.definitionElement.graphElements ++
        a.definitionElement.inputsSection.toSeq.flatMap(_.inputDeclarations) ++
        a.definitionElement.outputsSection.toSeq.flatMap(_.outputs)

    for {
      linkedGraph <- LinkedGraphMaker.make(nodes = graphNodeElements, typeAliases = a.typeAliases)
      womGraph <- makeWomGraph(linkedGraph, a.definitionElement.name)
    } yield WorkflowDefinition(a.definitionElement.name, womGraph, Map.empty, Map.empty)

  }

  private def makeWomGraph(linkedGraph: LinkedGraph, workflowName: String): ErrorOr[WomGraph] = {

    def graphNodeCreationFold(currentValidation: ErrorOr[List[GraphNode]], next: WorkflowGraphElement): ErrorOr[List[GraphNode]] = {
      currentValidation flatMap { currentList =>
        val availableValues: Map[String, OutputPort] = (for {
          node <- currentList
          port <- node.outputPorts
        } yield port.name -> port).toMap
        val nextGraphNodeValidation = WorkflowGraphElementToGraphNode.convert(GraphNodeMakerInputs(next, linkedGraph.consumedValueLookup, availableValues, linkedGraph.typeAliases, workflowName))
        nextGraphNodeValidation map { nextGraphNode => currentList :+ nextGraphNode }
      }
    }

    val graphNodesValidation = getOrdering(linkedGraph) flatMap { ordering: List[WorkflowGraphElement] =>
      ordering.foldLeft[ErrorOr[List[GraphNode]]](List.empty[GraphNode].validNel)(graphNodeCreationFold)
    }

    graphNodesValidation flatMap { graphNodes => WomGraph.validateAndConstruct(graphNodes.toSet) }
  }

  private def getOrdering(linkedGraph: LinkedGraph): ErrorOr[List[WorkflowGraphElement]] = {
    // Find the topological order in which we must create the graph nodes:
    val edges = linkedGraph.edges map { case LinkedGraphEdge(from, to) => DiEdge(from, to) }

    Graph.from[WorkflowGraphElement, DiEdge](linkedGraph.elements, edges).topologicalSort match {
      case Left(cycleNode) => s"This workflow contains a cyclic dependency on ${cycleNode.value}".invalidNel
        // This asInstanceOf is not required, but it suppresses an incorrect intelliJ error highlight:
      case Right(topologicalOrder) => topologicalOrder.toList.map(_.value).asInstanceOf[List[WorkflowGraphElement]].validNel
    }
  }
}
