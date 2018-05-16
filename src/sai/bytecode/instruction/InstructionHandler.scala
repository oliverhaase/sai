package bytecode.instruction


import org.apache.bcel.generic.NEW
import org.apache.bcel.generic.INVOKESPECIAL
import org.apache.bcel.generic.PUTFIELD
import org.apache.bcel.generic.DUP
import org.apache.bcel.generic.ALOAD
import org.apache.bcel.generic.ASTORE
import org.apache.bcel.generic.GETSTATIC
import org.apache.bcel.generic.PUTSTATIC
import org.apache.bcel.generic.GETFIELD
import sai.vm.ObjectRef
import sai.vm.StaticRef
import vm.Frame

sealed trait InstructionHandler[T <: org.apache.bcel.generic.Instruction] {
  def apply(frame: Frame, instruction: T): Frame
}

object InstructionHandler {
  def handle(frame: Frame, instruction: org.apache.bcel.generic.Instruction): Frame = {
    instruction match {
      case i: ALOAD => AloadHandler(frame, i)
      case i: ASTORE => AstoreHandler(frame, i)
      case i: DUP => DupHandler(frame, i)
      case i: GETFIELD => GetFieldHandler(frame, i)
      case i: GETSTATIC => GetStaticHandler(frame, i)
      case i: INVOKESPECIAL => InvokeSpecialHandler(frame, i)
      case i: NEW => NewHandler(frame, i)
      case i: PUTFIELD => PutField(frame, i)
      case i: PUTSTATIC => PutStatic(frame, i)
      case _ => frame
    }
  }
}

object AloadHandler extends InstructionHandler[ALOAD] {
  override def apply(frame: Frame, aload: ALOAD): Frame = {
    val objectRef = frame.localVars.get(aload.getIndex)
    val updatedStack = frame.opStack.push(objectRef)
    frame.copy(opStack = updatedStack)
  }
}

object AstoreHandler extends InstructionHandler[ASTORE] {
  def apply(frame: Frame, astore: ASTORE): Frame = {
    val (objectRef, updatedStack) = frame.opStack.pop
    val updatedLocalVars = frame.localVars.set(astore.getIndex, objectRef)
    frame.copy(opStack = updatedStack, localVars = updatedLocalVars)
  }
}

object DupHandler extends InstructionHandler[DUP] {
  override def apply(frame: Frame, dup: DUP): Frame = {
    val updatedStack = frame.opStack.dup
    frame.copy(opStack = updatedStack)
  }
}

object GetFieldHandler extends InstructionHandler[GETFIELD] {
  override def apply(frame: Frame, instruction: GETFIELD): Frame = {
    frame
  }
}

object GetStaticHandler extends InstructionHandler[GETSTATIC] {
  def apply(frame: Frame, getStatic: GETSTATIC): Frame = {
    val fieldName = getStatic.getFieldName(frame.cpg)
    val value = StaticRef(s"${frame.method.clazz.toString}/$fieldName")
    val updatedStack = frame.opStack.push(value)
    frame.copy(opStack = updatedStack)
  }
}

object InvokeSpecialHandler extends InstructionHandler[INVOKESPECIAL] {
  override def apply(frame: Frame, invokeSpecial: INVOKESPECIAL): Frame = {
    val (_, updatedStack) = frame.opStack.pop
    frame.copy(opStack = updatedStack)
  }
}

object NewHandler extends InstructionHandler[NEW] {
  override def apply(frame: Frame, newInstruction: NEW): Frame = {
    val objectType = newInstruction.getLoadClassType(frame.cpg)
    val lineNumber = frame.method.getLineNumber(newInstruction)
    val objectRef = ObjectRef(objectType, s"O$lineNumber")
    val updatedStack = frame.opStack.push(objectRef)
    frame.copy(opStack = updatedStack)
  }
}

object PutField extends InstructionHandler[PUTFIELD] {
  override def apply(frame: Frame, putField: PUTFIELD): Frame = {
    val (value, tmpStack) = frame.opStack.pop
    val (reference, updatedStack) = tmpStack.pop
    (value, reference) match {
      case (_, _ @ ObjectRef(_, _)) =>
        frame.copy(opStack = updatedStack)
      case (_, _ @ StaticRef(_)) =>
        frame.copy(opStack = updatedStack)
    }
  }
}

object PutStatic extends InstructionHandler[PUTSTATIC] {
  override def apply(frame: Frame, putStatic: PUTSTATIC): Frame = {
    frame
  }
}
