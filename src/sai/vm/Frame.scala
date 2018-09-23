package vm

import cg.{PhantomReferenceNode, _}
import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import sai.vm.{LocalVars, OpStack, Reference}

case class Frame(method: Method,
                 cpg: ConstantPoolGen,
                 stack: OpStack,
                 localVars: LocalVars,
                 cg: ConnectionGraph,
                 cache: Map[Method, ConnectionGraph] = Map.empty[Method, ConnectionGraph]) {
}

object Frame {

  def apply(method: Method): Frame = {
    val (localVars, cg) = transferActuals(method)
    new Frame(method, method.cpg, OpStack(), localVars, cg)
  }

  private def transferActuals(method: Method) = {

    val actuals = method.inputReferences

    // create phantom nodes for actuals
    val phantoms = for {
      (_, Reference(_, actual: ActualReferenceNode)) <- actuals
      phantom                                        = PhantomReferenceNode(actual)
    } yield phantom

    val phantomEscapes = for {
      phantom <- phantoms
    } yield phantom -> ArgEscape

    // create local nodes for actuals
    val formalReferences = for {
      (index, Reference(refType, actual: ActualReferenceNode)) <- actuals
      localReference                                           = Reference(refType, LocalReferenceNode(actual))
    } yield index -> localReference

    val formals = for {
      (_, Reference(_, formal)) <- formalReferences
    } yield formal

    val formalEscapes = for {
      formal <- formals
    } yield formal -> NoEscape

    // link local nodes with phantom nodes
    val edges = for {
      (formal: LocalReferenceNode, phantom: PhantomReferenceNode) <- formals.zip(phantoms)
      edge                                                        = DeferredEdge(formal -> phantom)
    } yield edge

    val localVars = LocalVars(method.maxLocals, formalReferences)
    val cg = ConnectionGraph(formals.toSet ++ phantoms.toSet,
                             edges.toSet,
                             (formalEscapes ++ phantomEscapes).toMap)
    (localVars, cg)
  }
}
