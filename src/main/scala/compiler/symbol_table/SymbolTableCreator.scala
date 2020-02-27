package compiler.symbol_table

import compiler.ast.AST.{ClassModel, DoBlock, Expression, Field, For, Inline, IntConst, Method, Model, Statement}
import sun.tools.tree.IfStatement

object SymbolTableCreator {

  def genSymbolTable(module: compiler.ast.AST.Module): SymbolTable ={
    val symbolTable = new SymbolTable
    module.models.foreach(genSymbolTable(symbolTable, _))
    symbolTable
  }

  def genSymbolTable(symbolTable: SymbolTable, model: Model): Unit ={
    model match {
      case classModel: ClassModel => {
        classModel.fields.foreach(genSymbolTable(symbolTable, _))
        classModel.methods.foreach(method => genSymbolTable(symbolTable, method.asInstanceOf[Method]))
      }
    }
  }

  def genSymbolTable(symbolTable: SymbolTable, field: Field): Unit ={

  }

  def genSymbolTable(symbolTable: SymbolTable, method: Method):Unit ={
    val identifier = method.name.value
    val symbolTableRow = new SymbolTableRow(identifier, -1, "Unit", MethodStructure)
    symbolTable.getInnerSymbolTable().addRow(identifier, symbolTableRow)

    method.body match {
      case inline: Inline => {
        genSymbolTable(symbolTable, inline)
      }
      case doBlock: DoBlock => {
        genSymbolTable(symbolTable, doBlock)
      }
    }
  }

  def genSymbolTable(symbolTable: SymbolTable, inline: Inline): Unit ={
    genSymbolTable(symbolTable, inline.expression)
  }


  def genSymbolTable(symbolTable: SymbolTable, doBlock: DoBlock): Unit ={
    doBlock.statement.foreach(statement => {
      genSymbolTable(symbolTable, statement)
    })
  }

  def genSymbolTable(symbolTable: SymbolTable, expression: Expression): Unit = {
    expression match {
      case _: IntConst =>
    }
  }

  def genSymbolTable(symbolTable: SymbolTable, statement: Statement): Unit = {
    statement match {
      case _: For =>
    }
  }
}
