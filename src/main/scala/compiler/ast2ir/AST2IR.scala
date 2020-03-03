package compiler.ast2ir

import compiler.ast.AST.{Add, _}
import compiler.ir.IR._
import compiler.symboltable.SymbolTable

object AST2IR {

  def moduleToIR(symbolTable: SymbolTable, module: compiler.ast.AST.Module): CompilationUnitIR = {
    CompilationUnitIR(module.models.map(model => modelToIR(symbolTable, model)))
  }

  def modelToIR(symbolTable: SymbolTable, model: Model): ModelIR = {
    model match {
      case classModel: ClassModel => classModelToIR(symbolTable, classModel)
      case objectModel: ObjectModel => objectModelToIR(symbolTable, objectModel)
    }
  }

  def classModelToIR(symbolTable: SymbolTable, model: ClassModel): ModelIR = {
    val methods = model.methods.map(method => methodToIR(symbolTable, method.asInstanceOf[Method]))

    ModelIR(ClassModelTypeIR, List(), model.name.value, None, List(), List(), methods)
  }

  def objectModelToIR(symbolTable: SymbolTable, model: ObjectModel): ModelIR = {

    val fields: List[FieldIR] = FieldIR(List(FinalIR, PrivateIR), "instance", model.name.value) +: model.fields.map(field => fieldToIR(symbolTable, field)).toList

    val methods = model.methods.map(method => methodToIR(symbolTable, method.asInstanceOf[Method]))

    val constructor = MethodIR(List(PublicIR), "<init>", "V", List(), List(ALoad(0), InvokeSpecial("java/lang/Object", "<init>", "()V"), ReturnIR))

    val staticInitializationBlock = MethodIR(List(PublicIR, StaticIR), "<clinit>", "V", List(), List(New(s"${model.name.value}"), Dup, InvokeSpecial(s"${model.name.value}", "<init>", "()V"), PutStatic(model.name.value, "instance", model.name.value), ReturnIR))

    ModelIR(ObjectModelTypeIR, List(), model.name.value, None, List(), fields, staticInitializationBlock +: constructor +: methods)
  }

  def fieldToIR(symbolTable: SymbolTable, field: Field): FieldIR = {
    FieldIR(List(), field.name.value, "()"+field.classType.value)
  }

  def methodToIR(symbolTable: SymbolTable, method: Method): MethodIR = {
    val innerSymbolTable = symbolTable.getInnerSymbolTable()
    val instructions = blockToIR(innerSymbolTable, method.body)

    val modifiers = method.modifiers.map {
      case Private => PrivateIR
      case Protected => ProtectedIR
    }.toList ++ (if (!method.modifiers.exists(modifier => modifier.equals(Public))) List(PublicIR) else List())

    val parameters = method.parameters.map(parameter => ParameterIR(List(), parameter.name.value, parameter.classType.value)).toList

    MethodIR(modifiers, method.name.value, "V", parameters, instructions)
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

        if (identifier.isEmpty) {
          throw new Exception(s"Identifier ${assign.name.value} not found in symbol table: $symbolTable")
        } else {
          val instructions: List[InstructionIR] = blockToIR(symbolTable, assign.block) :+ IStore(identifier.get.id)
          instructions
        }
      }
    }
  }

  def getFullClassLocation(symbolTable: SymbolTable, className: String): Option[String] ={
      val importLocations = symbolTable.getImport(className).mkString("/")
  }

}
