
package compiler.ast

object IR {

  case class CompilationUnitIR(classes: List[ClassIR])

  case class ClassIR(modifiers: List[ModifierIR], identifier: String, extendedBy: String, interfaces: List[String], fields: List[FieldIR], methods: List[MethodIR])

  trait ModifierIR

  case object PublicIR extends ModifierIR
  case object ProtectedIR extends ModifierIR
  case object PrivateIR extends ModifierIR

  case class FieldIR(modifiers: List[ModifierIR], identifier: String, `type`: String)

  case class MethodIR(modifiers: List[ModifierIR], identifier: String, `type`: String, parameters: List[ParameterIR], instructions: List[InstructionIR])

  case class ParameterIR(modifiers: List[ModifierIR], identifier: String, `type`: String)

  case class InstructionsIR(instructions: List[InstructionIR])

  trait InstructionIR
}
