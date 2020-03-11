package compiler.codegen

import compiler.ir.IR._
import javassist.bytecode.{Bytecode, Opcode}

object InstructionCodeGen {

  def genInstructionCode(bytecode: Bytecode, instruction: InstructionIR): Unit = {
    instruction match {
      case Dup => bytecode.add(Opcode.DUP)
      case newInstance: New => bytecode.addNew(newInstance.clazz)
      case aLoad: ALoad => bytecode.addAload(aLoad.value)
      case IAdd => bytecode.add(Opcode.IADD)
      case ISubtract => bytecode.add(Opcode.ISUB)
      case IMultiply => bytecode.add(Opcode.IMUL)
      case IDivide => bytecode.add(Opcode.IDIV)
      case iConst0: IConst0 => bytecode.addIconst(iConst0.value)
      case IStore(id) => bytecode.addIstore(id)
      case invokeVirtual: InvokeVirtual => bytecode.addInvokevirtual(invokeVirtual.clazz, invokeVirtual.methodName, invokeVirtual.description)
      case invokeSpecial: InvokeSpecial => bytecode.addInvokespecial(invokeSpecial.clazz, invokeSpecial.methodName, invokeSpecial.description)
      case getStatic: GetStatic => bytecode.addGetstatic(getStatic.internalName, getStatic.staticVariableName, getStatic.typeDescriptor)
      case putStatic: PutStatic => bytecode.addPutstatic(putStatic.fieldLocation, putStatic.fieldName, putStatic.clazz)
      case AReturnIR => bytecode.addOpcode(Opcode.ARETURN)
      case ReturnIR => bytecode.addReturn(null)
      case maxLocals: MaxLocals => bytecode.setMaxLocals(maxLocals.value)
    }
  }
}
