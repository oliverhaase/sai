package vm.interpreter

import ea._
import org.apache.bcel.generic.ObjectType
import sai.bytecode.Program

object Helper {

  /**
    * Arrays are treated like ordinary objects.
    * Since we don't know which index is used to access an array,
    * all fields are grouped under one field named "[i]".
    */
  val arrayFieldName = "[i]"

  /**
    * Find all object nodes p points to.
    * If there are no object nodes, a phantom object node will be created and added to the cg.
    */
  def getPointsToOrCreatePhantomObject(cg: ConnectionGraph,
                                       p: ReferenceNode): (Set[ObjectNode], ConnectionGraph) = {
    cg.pointsTo(p) match {
      case nodes if nodes.nonEmpty =>
        (nodes, cg)
      case _ =>
        val phantomObjectNode = PhantomObjectNode(p)
        val updatedCG =
          cg.addNode(phantomObjectNode)
            .addEdge(p -> phantomObjectNode)
            .updateEscapeState(phantomObjectNode -> ArgEscape)
        (Set(phantomObjectNode), updatedCG)
    }
  }

  /**
    * Determine escape state for a given object type.
    * If an object implements the runnable interface,
    * the escape state is GlobalEscape,
    * otherwise the escape state is NoEscape.
    */
  def determineEscapeState(objectType: ObjectType): EscapeState = {
    val clazz             = Class.forName(objectType.getClassName)
    val interfaces        = clazz.getInterfaces
    val runnableInterface = classOf[java.lang.Runnable]
    if (interfaces.contains(runnableInterface) || overridesFinalizeMethod(objectType))
      GlobalEscape
    else
      NoEscape
  }

  def overridesFinalizeMethod(objectType: ObjectType): Boolean = {
    val clazz = Program.getClass(objectType.getClassName)
    // check if clazz or any superclass (other than 'Object' = dropRight(1)) overrides the finalize method
    (clazz :: clazz.superClasses)
      .dropRight(1)
      .flatMap(_.methods)
      .exists(_.name == "finalize")
  }

  /**
    * Get or create field node
    * @return tuple with a reference to the field node and the updated connection graph.
    */
  def getOrCreateFieldNode(cg: ConnectionGraph,
                           o: ObjectNode,
                           fieldname: String): (FieldReferenceNode, ConnectionGraph) = {
    val fieldNode = FieldReferenceNode(o, fieldname)
    val updatedCG =
      cg.addNode(fieldNode)
        .updateEscapeState(fieldNode -> NoEscape)
        .addEdge(o -> fieldNode)
    (fieldNode, updatedCG)
  }
}
