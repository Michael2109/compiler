package compiler.symboltable

import compiler.ast.AST._

object SymbolTableCreator {

  def genSymbolTable(module: compiler.ast.AST.Module): SymbolTable = {
    val symbolTable = new SymbolTable
    module.models.foreach(genSymbolTable(symbolTable, _))
    symbolTable
  }

  def genSymbolTable(symbolTable: SymbolTable, model: Model): Unit = {
    model match {
      case classModel: ClassModel => {
        classModel.fields.foreach(genSymbolTable(symbolTable, _))
        classModel.methods.foreach(method => genSymbolTable(symbolTable, method.asInstanceOf[Method]))
      }
      case objectModel: ObjectModel => {
        objectModel.fields.foreach(genSymbolTable(symbolTable, _))
        objectModel.methods.foreach(method => genSymbolTable(symbolTable, method.asInstanceOf[Method]))
      }
    }
  }

  def genSymbolTable(symbolTable: SymbolTable, field: Field): Unit = {

  }

  def genSymbolTable(symbolTable: SymbolTable, method: Method): Unit = {
    val innerSymbolTable = symbolTable.getInnerSymbolTable()
    val identifier = method.name.value
    val symbolTableRow = SymbolTableRow(identifier, -1, "Unit", MethodStructure)
    innerSymbolTable.addRow(identifier, symbolTableRow)

    method.body match {
      case inline: Inline => {
        genSymbolTable(innerSymbolTable, inline)
      }
      case doBlock: DoBlock => {
        genSymbolTable(innerSymbolTable, doBlock)
      }
    }
  }

  def genSymbolTable(symbolTable: SymbolTable, inline: Inline): Unit = {
    genSymbolTable(symbolTable, inline.expression)
  }


  def genSymbolTable(symbolTable: SymbolTable, doBlock: DoBlock): Unit = {
    doBlock.statement.foreach(statement => {
      genSymbolTable(symbolTable, statement)
    })
  }

  def genSymbolTable(symbolTable: SymbolTable, expression: Expression): Unit = {
    expression match {
      case _: IntConst =>
      case _: ABinary =>
    }
  }

  def genSymbolTable(symbolTable: SymbolTable, statement: Statement): Unit = {
    statement match {
      case _: For =>
      case Inline(expression) => genSymbolTable(symbolTable, expression)
      case Assign(name, t, immutable, block) => {
        val identifier = name.value
        val id = symbolTable.getNextElementId(VariableStructure)
        symbolTable.addRow(name.value, new SymbolTableRow(identifier, id, "Void", VariableStructure))
        genSymbolTable(symbolTable, block)
      }
    }
  }
}
