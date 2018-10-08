package vm.interpreter.impl

import cg.{ConnectionGraph, PhantomReturnNode}
import org.apache.bcel.classfile.{ConstantInvokeDynamic, ConstantNameAndType}
import org.apache.bcel.generic._
import sai.bytecode.{Clazz, Method, Program}
import sai.vm.{DontCare, Reference}
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}
import vm.{Frame, SummaryInformation}

private[interpreter] object InvokeInterpreter extends InterpreterBuilder[InvokeInstruction] {

  override def apply(i: InvokeInstruction): InstructionInterpreter = new InstructionInterpreter {
    override protected[interpreter] def doInterpret(frame: Frame): Frame = {
      val Frame(_, cpg, stack, _, _, _) = frame

      val clazzbane = i.getClassName(cpg)
      val name = i.getMethodName(cpg)
      val methodToInvoke = lookupMethodToInvoke(frame, i)

      // calculate summary for 'method to invoke'
      val resultSummary = calculateSummary(methodToInvoke, frame)
      val updatedCG     = frame.cg.merge(resultSummary)

      // fetch return value from method invocation
      val returnValue = resultSummary.nodes.find(node =>
        node.isInstanceOf[PhantomReturnNode] && node.id == methodToInvoke.id) match {
        case Some(returnNode) =>
          Some(Reference(i.getReturnType(cpg), returnNode))
        case None =>
          None
      }

      // push return value on stack (or fill up with DontCare if there is no return value)
      val updatedStack = if (returnValue.isDefined) {
        stack.pop(i.consumeStack(cpg)).push(returnValue.get)
      } else {
        stack.pop(i.consumeStack(cpg)).push(DontCare, i.produceStack(cpg))
      }

      frame.copy(stack = updatedStack, cg = updatedCG)
    }
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
        case _: INVOKESTATIC  =>
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
        val clazz      = Program.getClass(objectRef.get)
        val maybeClazz = (clazz :: clazz.superClasses).find(_.methods.exists(matches))
        if (maybeClazz.isDefined) {
          maybeClazz.get.lookupMethod(i.getMethodName(pool)).get
        } else {
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
          if (clazz.isInterface && objectClass.methods.exists(m => m.isPublic && matches(m))) {
            objectClass.methods.find(m => m.isPublic && matches(m)).get
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
            .find(_.methods.exists(m => !m.isAbstract && matches(m)))
            .flatMap(c => c.lookupMethod(i.getMethodName(pool)))
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
