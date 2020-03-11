package compiler.symboltable

import scala.collection.mutable.ListBuffer

class ClassSymbolTable extends SymbolTableNew {

  val fields: ListBuffer[FieldStructure] = ListBuffer()

  val methods: ListBuffer[MethodSymbolTable] = ListBuffer()

  def addField(field: FieldStructure): Unit ={
    fields += field
  }

  def addMethod(methodSymbolTable: MethodSymbolTable): Unit ={
    methods += methodSymbolTable
  }

}
