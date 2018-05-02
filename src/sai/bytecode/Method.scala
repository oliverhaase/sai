package sai.bytecode

import org.apache.bcel.Const
import org.apache.bcel.classfile.CodeException
import org.apache.bcel.classfile.Utility
import org.apache.bcel.generic.ConstantPoolGen
import org.apache.bcel.generic.InstructionList
import org.apache.bcel.generic.InstructionHandle
import org.apache.bcel.generic.ExceptionThrower
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.EntryPoint
import sai.bytecode.instruction.ExitPoint
import sai.vm.ObjectNode
import sai.vm.ParameterObject
import sai.vm.ThisObject

class Method (bcelMethod : org.apache.bcel.classfile.Method, cpg: ConstantPoolGen, clazz: Clazz) {
  val isAbstract = bcelMethod.isAbstract
  val isNative = bcelMethod.isNative
  val isDefined = !isAbstract && !isNative

  /**
   * Retrieve all instructions that handle exceptions thrown by the given instruction.
   * @param instruction which may throw exceptions
   * @return Set of instructions which handle potential exceptions.
   */
  def getCatchInstructions(instruction: InstructionHandle): Set[Instruction] = {

    // returns the first instruction within the catch block
    def getCatchInstruction(exceptionHandler: CodeException) = {
      instructions.find(_.pc.contains(exceptionHandler.getHandlerPC))
        .getOrElse(throw new RuntimeException(s"instruction for position ${instruction.getPosition} not found"))
    }

    // check if an exception handler is able to catch a specific exception
    def canCatch(exceptionHandler: CodeException, exception: Class[_]) = {
      val tryRange = Range(exceptionHandler.getStartPC, exceptionHandler.getEndPC)
      tryRange.contains(instruction.getPosition) && {
        val classContent = cpg.getConstantPool.getConstantString(exceptionHandler.getCatchType, Const.CONSTANT_Class)
        val className = Utility.compactClassName(classContent, /* remove prefix = */false)
        val handlerClass = Class.forName(className)
        handlerClass.isAssignableFrom(exception)
      }
    }

    instruction.getInstruction match {
      case thrower: ExceptionThrower =>
        val exceptionHandlers = bcelMethod.getCode.getExceptionTable
        thrower.getExceptions.foldLeft(Set[Instruction]()) { (acc, exception) =>
          val maybeHandler = exceptionHandlers.collectFirst {
            case exceptionHandler if canCatch(exceptionHandler, exception) => getCatchInstruction(exceptionHandler)
          }
          acc + maybeHandler.getOrElse(exitPoint)
        }
      case _ => Set()
    }
  }

  private def body(bcelInstructions: List[InstructionHandle]) =
     for ( bcelInstruction <- bcelInstructions )
       yield Instruction(bcelInstruction, cpg, this)

  private def decorate(body: List[Instruction]) =
    new EntryPoint(this) :: body ::: List(new ExitPoint(this))

  val instructions: List[Instruction] =
    if ( isDefined )
      decorate(body(new InstructionList(bcelMethod.getCode().getCode()).getInstructionHandles().toList))
    else
      Nil

  def exitPoint = instructions.last

  def firstInstruction = instructions(1)

  def lookup(bcelInstruction: org.apache.bcel.generic.InstructionHandle): Instruction =
    instructions find (_ encapsulates bcelInstruction) match {
      case Some(i) => i
      case None => throw new RuntimeException("instruction not found")
    }


  private def argReferences(index : Int, bcelArgs: List[org.apache.bcel.generic.Type]): Map[Int, ObjectNode] =
    if ( bcelArgs == Nil )
      Map()
    else
      bcelArgs.head match {
      case basicType: org.apache.bcel.generic.BasicType =>
        argReferences(index + basicType.getSize, bcelArgs.tail)
      case referenceType: org.apache.bcel.generic.ReferenceType =>
        argReferences(index + 1, bcelArgs.tail) + (index -> new ParameterObject(referenceType))
    }

  val inputReferences: Map[Int, ObjectNode] =
    if ( bcelMethod.isStatic )
      argReferences(0, bcelMethod.getArgumentTypes.toList)
    else
      argReferences(1, bcelMethod.getArgumentTypes.toList) + (0 -> new ThisObject(clazz.name))

  def maxLocals = bcelMethod.getCode.getMaxLocals

  def name = bcelMethod.getName
  override def toString = name

  def summary = exitPoint.statesOut

  def interpret {
      summary
      print
    }


  def print {
    println("." + toString + " " + inputReferences)
    instructions.foreach(instruction => instruction.print)
  }

}




