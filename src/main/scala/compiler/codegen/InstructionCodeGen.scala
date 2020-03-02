package compiler.codegen

import compiler.ir.IR._
import javassist.bytecode.{Bytecode, Opcode}

object InstructionCodeGen {

  def genInstructionCode(bytecode: Bytecode, instruction: InstructionIR): Unit = {
    instruction match {
      case aLoad: ALoad => bytecode.addAload(aLoad.value)
      case IAdd => bytecode.add(Opcode.IADD)
      case ISubtract => bytecode.add(Opcode.ISUB)
      case IMultiply => bytecode.add(Opcode.IMUL)
      case IDivide => bytecode.add(Opcode.IDIV)
      case iConst0: IConst0 => bytecode.addIconst(iConst0.value)
      case IStore(id) => bytecode.addIstore(id)
      case invokeSpecial: InvokeSpecial => bytecode.addInvokespecial(invokeSpecial.clazz, invokeSpecial.methodName, invokeSpecial.description)
      case ReturnIR => bytecode.addReturn(null)
      case maxLocals: MaxLocals => bytecode.setMaxLocals(maxLocals.value)
    }
  }
}
