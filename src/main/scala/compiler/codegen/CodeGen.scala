package compiler.codegen

import compiler.ir.IR._
import javassist.ClassPool
import javassist.bytecode._


object CodeGen {

  val version = 49


  def genCompilationUnitIRCode(compilationUnitIR: CompilationUnitIR): List[Array[Byte]] = {
    null
  }

  def genModelIRCode(modelIR: ModelIR): Array[Byte] = {

    val classPool = ClassPool.getDefault

    val classFile = new ClassFile(false, modelIR.name, null)

    modelIR.fields.foreach(field => genFieldIRCode(classFile, field))

    modelIR.methods.foreach(method => genMethodIRCode(classFile, method))

    classPool.makeClass(classFile).toBytecode
  }

  def genFieldIRCode(classFile: ClassFile, fieldIR: FieldIR): Unit = {
    val accessFlag = modifierIRsToAccessFlag(fieldIR.modifiers)

    val fieldInfo: FieldInfo = new FieldInfo(classFile.getConstPool, fieldIR.name, fieldIR.description)
    fieldInfo.setAccessFlags(accessFlag)
    classFile.addField(fieldInfo)
  }

  def genMethodIRCode(classFile: ClassFile, methodIR: MethodIR): Unit = {

    val modifiers: Int = modifierIRsToAccessFlag(methodIR.modifiers)

    val bytecode = new Bytecode(classFile.getConstPool)

    methodIR.instructions.foreach(instruction => {
      InstructionCodeGen.genInstructionCode(bytecode, instruction)
    })

    val methodInfo = new MethodInfo(classFile.getConstPool, methodIR.identifier, "()" + methodIR.`type`)
    methodInfo.setAccessFlags(modifiers)
    methodInfo.setCodeAttribute(bytecode.toCodeAttribute)

    classFile.addMethod(methodInfo)
  }

  private def modifierIRsToAccessFlag(modifierIRs: Seq[ModifierIR]): Int = {
    modifierIRs.map(modifierIRToAccessFlag).foldLeft(0)(_ | _)
  }

  private def modifierIRToAccessFlag(modifierIR: ModifierIR): Int = {
    modifierIR match {
      case StaticIR => AccessFlag.STATIC
      case FinalIR => AccessFlag.FINAL
      case PublicIR => AccessFlag.PUBLIC
      case PrivateIR => AccessFlag.PRIVATE
      case ProtectedIR => AccessFlag.PROTECTED
    }
  }
}
