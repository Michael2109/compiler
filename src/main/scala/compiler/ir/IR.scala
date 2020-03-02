package compiler.ir

object IR {

  case class CompilationUnitIR(classes: Seq[ModelIR])

  case class ModelIR(modelTypeIR: ModelTypeIR, modifiers: List[ModifierIR], name: String, extendedBy: Option[String], interfaces: List[String], fields: List[FieldIR], methods: Seq[MethodIR])

  trait ModelTypeIR

  case object ObjectModelTypeIR extends ModelTypeIR
  case object ClassModelTypeIR extends ModelTypeIR

  trait ModifierIR

  case object PublicIR extends ModifierIR

  case object ProtectedIR extends ModifierIR

  case object PrivateIR extends ModifierIR

  case class FieldIR(modifiers: Seq[ModifierIR], identifier: String, `type`: String)

  case class MethodIR(modifiers: Seq[ModifierIR], identifier: String, `type`: String, parameters: List[ParameterIR], instructions: List[InstructionIR])

  case class ParameterIR(modifiers: Seq[ModifierIR], identifier: String, `type`: String)

  trait InstructionIR

  case class ALoad(value: Int) extends InstructionIR

  case class InvokeSpecial(clazz: String, methodName: String, description: String) extends InstructionIR

  case object ReturnIR extends InstructionIR

  case class MaxLocals(value: Int) extends InstructionIR

  case class IStore(id: Int) extends InstructionIR

  case class IConst0(value: Int) extends InstructionIR

  case object IAdd extends InstructionIR

  case object ISubtract extends InstructionIR

  case object IMultiply extends InstructionIR

  case object IDivide extends InstructionIR

}
