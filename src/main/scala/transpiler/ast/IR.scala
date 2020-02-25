
package transpiler.ast

object IR {

  case class CompilationUnitIR(classes: List[ClassIR])

  case class ClassIR(isFinal: Boolean, identifier: String, extendedBy: String, interfaces: List[String], fields: FieldsIR)

  case class FieldsIR(fields: List[FieldIR])

  case class FieldIR(modifierIR: ModifierIR, identifier: String, `type`: String)

  trait ModifierIR

  case object PublicIR extends ModifierIR
  case object ProtectedIR extends ModifierIR
  case object PrivateIR extends ModifierIR

}
