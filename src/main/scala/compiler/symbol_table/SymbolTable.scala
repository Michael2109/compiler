package compiler.symbol_table

import compiler.ast.AST.Module

import scala.collection.mutable.HashMap

class SymbolTable {

  private lazy val innerSymbolTable: SymbolTable = new SymbolTable

  private val identifierToRow: HashMap[String, SymbolTableRow] = HashMap()

  def getInnerSymbolTable(): SymbolTable = {
    innerSymbolTable
  }

  def addRow(identifier: String, symbolTableRow: SymbolTableRow): Unit = {
    identifierToRow += (identifier -> symbolTableRow)
  }

  def findIdentifier(identifier: String): Option[SymbolTableRow] = {
    identifierToRow.get(identifier)
  }
}

class SymbolTableRow(identifier: String, id: Int, returnType: String, structureType: StructureType)

trait StructureType
case object ClassStructure extends StructureType
case object MethodStructure extends StructureType
case object FieldStructure extends StructureType
case object VariableStructure extends StructureType
