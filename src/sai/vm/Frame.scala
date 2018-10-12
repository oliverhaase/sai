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
                 summaryCache: Map[Method, ConnectionGraph] = Map.empty[Method, ConnectionGraph]) {}

object Frame {

  def apply(method: Method): Frame = {
    val (localVars, cg) = createPhantomReferences(method)
    new Frame(method, method.cpg, OpStack(), localVars, cg)
  }

  private def createPhantomReferences(method: Method) = {

    val formalReferences = method.inputReferences

    val formals = for {
      (_, Reference(_, formal: LocalReferenceNode)) <- formalReferences
    } yield formal

    val formalEscapes = for {
      formal <- formals
    } yield formal -> NoEscape

    // create phantom nodes for actuals
    val phantoms = for {
      formal  <- formals
      phantom = PhantomReferenceNode(formal)
    } yield phantom

    val phantomEscapes = for {
      phantom <- phantoms
    } yield phantom -> ArgEscape

    // link local nodes with phantom nodes
    val edges = for {
      (formal, phantom) <- formals.zip(phantoms)
      edge              = DeferredEdge(formal -> phantom)
    } yield edge

    val localVars = LocalVars(method.maxLocals, formalReferences)
    val cg = ConnectionGraph(formals.toSet ++ phantoms.toSet,
                             edges.toSet,
                             (formalEscapes ++ phantomEscapes).toMap)
    (localVars, cg)
  }
}
