package compiler.symboltable

import scala.collection.mutable

class SymbolTable(outerSymbolTable: Option[SymbolTable]) {

  private lazy val innerSymbolTable: SymbolTable = new SymbolTable(Some(this))

  private val imports: mutable.Map[String, List[String]] = mutable.HashMap()

  private val identifierToRow: mutable.Map[String, SymbolTableRow] = mutable.LinkedHashMap()

  def getOuterSymbolTable(): Option[SymbolTable] = {
    outerSymbolTable
  }

  def getInnerSymbolTable(): SymbolTable = {
    innerSymbolTable
  }

  def addImport(className: String, locations: List[String]): Unit = {
    imports.put(className, locations)
  }

  def getImport(className: String): Option[List[String]] = {
    imports.get(className) match {
      case found: Some[List[String]] => found
      case None => getOuterSymbolTable() match {
        case Some(table) => table.getImport(className)
        case None => None
      }
    }
  }

  def addRow(identifier: String, symbolTableRow: SymbolTableRow): Unit = {
    identifierToRow += (identifier -> symbolTableRow)
  }

  def findIdentifier(identifier: String): Option[SymbolTableRow] = {
    identifierToRow.get(identifier) match {
      case found: Some[SymbolTableRow] => found
      case None => getOuterSymbolTable() match {
        case Some(table) => table.findIdentifier(identifier)
        case None => None
      }
    }
  }

  def getElementsSize(): Int = {
    identifierToRow.size
  }

  def getNextElementId(structureType: StructureType): Int = {
    identifierToRow.values.count(v => v.structureType == structureType) + 1
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
