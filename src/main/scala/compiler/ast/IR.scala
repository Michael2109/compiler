
package compiler.ast

object IR {

  case class CompilationUnitIR(classes: List[ClassIR])

  case class ClassIR(modifiers: ModifiersIR, identifier: String, extendedBy: String, interfaces: List[String], fields: FieldsIR, methods: MethodsIR)

  case class ModifiersIR(modifiers: List[ModifierIR])

  trait ModifierIR

  case object PublicIR extends ModifierIR
  case object ProtectedIR extends ModifierIR
  case object PrivateIR extends ModifierIR

  case class FieldsIR(modifiers: ModifiersIR, fields: List[FieldIR])

  case class FieldIR(modifiers: ModifiersIR, identifier: String, `type`: String)

  case class MethodsIR(methods: List[MethodIR])

  case class MethodIR(modifiers: ModifiersIR, identifier: String, `type`: String, parameters: ParametersIR, instructions: InstructionsIR)

  case class ParametersIR(parameters: List[ParameterIR])
  case class ParameterIR(modifiers: ModifiersIR, identifier: String, `type`: String)

  case class InstructionsIR(instructions: List[InstructionIR])

  trait InstructionIR
}
