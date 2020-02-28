package compiler.symboltable

import scala.collection.mutable

class SymbolTable {

  private lazy val innerSymbolTable: SymbolTable = new SymbolTable

  private val identifierToRow: mutable.Map[String, SymbolTableRow] = mutable.LinkedHashMap()

  def getInnerSymbolTable(): SymbolTable = {
    innerSymbolTable
  }

  def addRow(identifier: String, symbolTableRow: SymbolTableRow): Unit = {
    identifierToRow += (identifier -> symbolTableRow)
  }

  def findIdentifier(identifier: String): Option[SymbolTableRow] = {
    identifierToRow.get(identifier)
  }
  def getElementsSize(): Int ={
    identifierToRow.size
  }

  override def toString: String = {
    identifierToRow.values.map(x => x.toString).mkString(System.lineSeparator())
  }
}

case class SymbolTableRow(identifier: String, id: Int, returnType: String, structureType: StructureType)

trait StructureType
case object ClassStructure extends StructureType
case object MethodStructure extends StructureType
case object FieldStructure extends StructureType
case object VariableStructure extends StructureType
