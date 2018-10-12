package vm.interpreter.impl

import ea.{ArgEscape, ConnectionGraph, PhantomReturnNode, SummaryInformation}
import org.apache.bcel.Const
import org.apache.bcel.classfile.{ConstantInvokeDynamic, ConstantNameAndType}
import org.apache.bcel.generic.{BasicType, _}
import sai.bytecode.{Clazz, Method, Program}
import sai.vm.{DontCare, OpStack, Reference}
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}
import vm.Frame

private[interpreter] object InvokeInterpreter extends InterpreterBuilder[InvokeInstruction] {

  override def apply(i: InvokeInstruction): InstructionInterpreter = new InstructionInterpreter {
    override protected[interpreter] def doInterpret(inFrame: Frame): Frame = {
      val frame = inFrame

      val methodToInvoke = lookupMethodToInvoke(frame, i)

      // calculate summary for 'method to invoke'
      val resultSummary = calculateSummary(methodToInvoke, frame)
      var updatedCG     = frame.cg.merge(resultSummary)

      // update the stack
      val updatedStack = i.getReturnType(frame.cpg) match {
        case basicType: BasicType =>
          frame.stack.pop(i.consumeStack(frame.cpg)).push(DontCare, basicType.getSize)
        case objectType: ObjectType =>
          val returnNode = findOrCreateReturnNode(resultSummary, methodToInvoke)
          if (!updatedCG.nodes.contains(returnNode))
            updatedCG = updatedCG.addNode(returnNode).updateEscapeState(returnNode -> ArgEscape)
          frame.stack.pop(i.consumeStack(frame.cpg)).push(Reference(objectType, returnNode))
      }

      frame.copy(stack = updatedStack, cg = updatedCG)
    }
  }

  def findOrCreateReturnNode(summary: ConnectionGraph,
                             method: Method): PhantomReturnNode = {
    summary.nodes
      .find(n => n.id == method.id && n.isInstanceOf[PhantomReturnNode])
      .map(_.asInstanceOf[PhantomReturnNode])
      .getOrElse(new PhantomReturnNode(method.id))
  }

  /**
    * Lookup "the real" method to invoke based on a generic invoke instruction 'i'.
    * The JVMS defines different lookup strategies for invoke instructions.
    */
  private def lookupMethodToInvoke(frame: Frame, i: InvokeInstruction): Method = {
    val pool = frame.cpg

    // invokespecial, invokeinterface, and invokevirtual are invoked with an object reference
    // invokedynamic and invokestatic have no object reference
    val objectRef = {
      i match {
        case _: INVOKEDYNAMIC =>
          None
        case _: INVOKESTATIC =>
          None
        case _ =>
          val updatedStack = frame.stack.pop(i.consumeStack(pool) - 1)
          val ref =
            updatedStack.peek
              .asInstanceOf[Reference]
              .referenceType
              .asInstanceOf[ObjectType]
          Some(ref.getClassName)
      }
    }

    // check if a method matches the resolved method name & method signature
    def matches(m: Method): Boolean = {
      m.signature == i.getSignature(pool) && m.name == i.getMethodName(pool)
    }

    // lookup
    i match {
      case _: INVOKEDYNAMIC =>
        throw new NotImplementedError("INVOKEDYNAMIC is not supported")
      case _: INVOKEINTERFACE =>
        // As a first step, we query the class of the object reference which is on the stack.
        // We then check if this class contains a method which matches the signature and the method name of the requested method.
        // If this is true, we found the method to invoke.
        val clazz = Program.getClass(objectRef.get)
        // In case this class does not contain a matching method, we check the class's superclass for a match.
        // If that class does also not contain a matching method, we repeat the process with that class's superclass and so on.
        // We stop in case we find a matching method.
        val maybeClazz = (clazz :: clazz.superClasses).find(_.methods.exists(matches))
        if (maybeClazz.isDefined) {
          maybeClazz.get.lookupMethod(i.getMethodName(pool)).get
        } else {
          // It is possible that we do not find a matching method this way.
          // If so, we check all interfaces the class of the object reference implements.
          // Any of these interfaces must define a non-abstract method which matches the signature and the name.
          // We then call the method of exactly that interface which contains the matching method.
          clazz.interfaces
            .find(_.methods.exists(m => !m.isAbstract && matches(m)))
            .flatMap(_.lookupMethod(i.getMethodName(pool)))
            .get
        }
      case _: INVOKESPECIAL =>
        val clazz = Program.getClass(i.getClassName(pool))
        val maybeClazz = (clazz :: clazz.superClasses).find(_.methods.exists(m =>
          m.isInstanceMethod && matches(m)))
        if (maybeClazz.isDefined) {
          maybeClazz.get.lookupMethod(i.getMethodName(pool)).get
        } else {
          val objectClass = Program.getClass(classOf[java.lang.Object].getName)
          if (clazz.isInterface && objectClass.methods.exists(
                m => m.isPublic && m.isInstanceMethod && matches(m))) {
            objectClass.methods.find(m => m.isPublic && m.isInstanceMethod && matches(m)).get
          } else {
            clazz.interfaces
              .find(_.methods.exists(m => !m.isAbstract && matches(m)))
              .flatMap(c => c.lookupMethod(i.getMethodName(pool)))
              .get
          }
        }
      case _: INVOKESTATIC =>
        Program.getClass(i.getClassName(pool)).lookupMethod(i.getMethodName(pool)).get
      case _: INVOKEVIRTUAL =>
        val clazz      = Program.getClass(objectRef.get)
        val maybeClazz = (clazz :: clazz.superClasses).find(_.methods.exists(matches))
        if (maybeClazz.isDefined) {
          maybeClazz.get.lookupMethod(i.getMethodName(pool)).get
        } else {
          clazz.interfaces
            .find(interface => interface.methods.exists(m => !m.isAbstract && matches(m)))
            .flatMap(interface => interface.lookupMethod(i.getMethodName(pool)))
            .get
        }
    }
  }

  def calculateSummary(methodToInvoke: Method, frame: Frame): ConnectionGraph = {
    val Frame(currentMethod, _, _, _, _, summaryCache) = frame

    // get all successors recursive
    val recursiveSuccessors = methodToInvoke.callGraph.recursive()

    // check if call of 'methodToInvoke' causes 'currentMethod' to be called -> endless recursion!
    val isRecursive = recursiveSuccessors.contains(currentMethod)

    if (!methodToInvoke.isDefined) {
      ConnectionGraph.empty()
    } else if (!isRecursive) {
      methodToInvoke.summary
    } else {
      val cgBefore =
        summaryCache.getOrElse(currentMethod, frame.cg.merge(currentMethod.nonRecursiveSummary))
      val cgMethodToInvoke =
        summaryCache.getOrElse(methodToInvoke, methodToInvoke.nonRecursiveSummary)
      val cgAfter = cgBefore.merge(cgMethodToInvoke)

      if (cgBefore == cgAfter && summaryCache.contains(methodToInvoke)) {
        summaryCache(methodToInvoke)
      } else {
        val initialFrame =
          Frame(methodToInvoke).copy(summaryCache = summaryCache + (currentMethod -> cgAfter))
        SummaryInformation(initialFrame,
                           methodToInvoke.controlFlowGraph,
                           findSuccessors = block => block.successors,
                           findPredecessors = block => block.predecessors)
      }
    }
  }

}
