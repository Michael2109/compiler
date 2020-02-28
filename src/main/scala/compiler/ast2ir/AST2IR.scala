package compiler.ast2ir

import compiler.ast.AST.{Add, _}
import compiler.ir.IR._
import compiler.symboltable.SymbolTable

object AST2IR {

  def moduleToIR(symbolTable: SymbolTable, module: compiler.ast.AST.Module): CompilationUnitIR = {

    CompilationUnitIR(module.models.map(model => model match {
      case classModel: ClassModel => modelToIR(symbolTable, classModel)
    }))
  }

  def modelToIR(symbolTable: SymbolTable, model: ClassModel): ClassIR = {
    val methods = model.methods.map(method => methodToIR(symbolTable, method.asInstanceOf[Method]))

    ClassIR(List(), model.name.value, None, List(), List(), methods)

  }

  def methodToIR(symbolTable: SymbolTable, method: Method): MethodIR = {
    val innerSymbolTable = symbolTable.getInnerSymbolTable()
    val instructions = blockToIR(innerSymbolTable, method.body)

    MethodIR(List(), method.name.value, "V", List(), instructions)
  }

  def blockToIR(symbolTable: SymbolTable, block: Block): List[InstructionIR] = {
    block match {
      case Inline(expression) => expressionToIR(symbolTable, expression)
      case DoBlock(statements) => statements.flatMap(statement => statementToIR(symbolTable, statement)).toList
    }
  }

  def expressionToIR(symbolTable: SymbolTable, expression: Expression): List[InstructionIR] = {
    expression match {
      case IntConst(value) => List(IConst0(value.intValue))
      case ABinary(op, expr1, expr2) => {

        expressionToIR(symbolTable, expr1) ++
          expressionToIR(symbolTable, expr2) :+
          opToIR(symbolTable, op)

      }
    }
  }

  def opToIR(symbolTable: SymbolTable, op: Operator): InstructionIR = {
    op match {
      case Add => IAdd
      case Subtract => ISubtract
      case Multiply => IMultiply
      case Divide => IDivide
    }
  }

  def statementToIR(symbolTable: SymbolTable, statement: Statement): List[InstructionIR] = {

    statement match {
      case assign: Assign => {
        val identifier = symbolTable.findIdentifier(assign.name.value)

        if(identifier.isEmpty){
          println(symbolTable)
          throw new Exception(s"Identifier ${assign.name.value} not found")
        }else {
          val instructions: List[InstructionIR] = blockToIR(symbolTable, assign.block) :+ IStore(identifier.get.id)
          instructions
        }
      }
    }
  }

}
