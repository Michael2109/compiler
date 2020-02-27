package compiler.ast2ir

import compiler.ast.AST.{Add, _}
import compiler.ir.IR._

object AST2IR {

  def moduleToIR(module: compiler.ast.AST.Module): CompilationUnitIR = {

    CompilationUnitIR(module.models.map(model => model match {
      case classModel: ClassModel => modelToIR(classModel)
    }))
  }

  def modelToIR(model: ClassModel): ClassIR = {
    val methods = model.methods.map(method => methodToIR(method.asInstanceOf[Method]))

    ClassIR(List(), model.name.value, None, List(), List(), methods)

  }

  def methodToIR(method: Method): MethodIR = {
    val instructions = blockToIR(method.body)

    MethodIR(List(), method.name.value, "V", List(), instructions)
  }

  def blockToIR(block: Block): List[InstructionIR] = {
    block match {
      case Inline(expression) => expressionToIR(expression)
      case DoBlock(statements) => statements.flatMap(statement => statementToIR(statement)).toList
    }
  }

  def expressionToIR( expression: Expression): List[InstructionIR] = {
    expression match {
      case IntConst(value) =>List(IConst0(value.intValue))
      case ABinary(op, expr1, expr2) => {

          expressionToIR( expr1) ++
          expressionToIR( expr2) :+
          opToIR(op)

      }
    }
  }

  def opToIR(op: Operator): InstructionIR ={
    op match {
      case Add => IAdd
      case Subtract => ISubtract
      case Multiply => IMultiply
      case Divide => IDivide
    }
  }

  def statementToIR(statement: Statement): List[InstructionIR] = {

    statement match {
      case assign: Assign => {
        val instructions: List[InstructionIR] = blockToIR(assign.block) :+ IStore(1)
        instructions
      }
    }
  }

}
