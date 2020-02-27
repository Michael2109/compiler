package compiler.symbol_table

import compiler.ast.AST.Module

object SymbolTable {
  def createSymbolTable(module: Module): SymbolTable ={
     new SymbolTable()
  }
}

class SymbolTable {

  val nestedSymbolTable = null

}

