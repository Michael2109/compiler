package compiler.ast2ir

import compiler.ast.AST.{Add, _}
import compiler.ast.IRUtils
import compiler.ir.IR._
import compiler.symboltable.{MethodStructure, SymbolTable}

object AST2IR {

  def moduleToIR(symbolTable: SymbolTable, module: compiler.ast.AST.Module): CompilationUnitIR = {
    CompilationUnitIR(nameSpaceToIR(symbolTable, module.header.nameSpace), module.models.map(model => modelToIR(symbolTable, model)))
  }

  def nameSpaceToIR(symbolTable: SymbolTable, nameSpace: NameSpace): NameSpaceIR = {
    NameSpaceIR(nameSpace.nameSpace.map(_.value).toList)
  }

  def modelToIR(symbolTable: SymbolTable, model: Model): ModelIR = {
    model match {
      case classModel: ClassModel => classModelToIR(symbolTable, classModel)
      case objectModel: ObjectModel => objectModelToIR(symbolTable, objectModel)
    }
  }

  def classModelToIR(symbolTable: SymbolTable, model: ClassModel): ModelIR = {
    val methods = model.methods.map(method => methodToIR(symbolTable, method.asInstanceOf[Method], false))

    ModelIR(ClassModelTypeIR, List(), model.name.value, None, List(), List(), methods)
  }

  def objectModelToIR(symbolTable: SymbolTable, model: ObjectModel): ModelIR = {

    symbolTable.addImport(model.name.value, List(model.name.value))

    val fields: List[FieldIR] = FieldIR(List(FinalIR, PrivateIR), "instance$", model.name.value) +: model.fields.map(field => fieldToIR(symbolTable, field)).toList

    val methods = model.methods.map(method => methodToIR(symbolTable, method.asInstanceOf[Method], true))

    val constructor = MethodIR(List(PublicIR), "<init>", "V", List(), List(ALoad(0), InvokeSpecial("java/lang/Object", "<init>", "()V"), ReturnIR))

    val modelInternalName = IRUtils.getInternalName(symbolTable, model.name.value).get

    val modelTypeDescriptor =  IRUtils.getTypeDescriptor(symbolTable, model.name.value).get

    val getInstanceMethod = MethodIR(List(PublicIR, StaticIR), "getInstance$", model.name.value, List(), List(GetStatic(modelInternalName, "instance$", modelTypeDescriptor), AReturnIR))

    val staticInitializationBlock = MethodIR(List(PublicIR, StaticIR), "<clinit>", "V", List(), List(New(s"${model.name.value}"), Dup, InvokeSpecial(s"${model.name.value}", "<init>", "()V"), PutStatic(model.name.value, "instance", model.name.value), ReturnIR))

    ModelIR(ObjectModelTypeIR, List(), model.name.value, None, List(), fields, getInstanceMethod +: staticInitializationBlock +: constructor +: methods)
  }

  def fieldToIR(symbolTable: SymbolTable, field: Field): FieldIR = {
    FieldIR(List(), field.name.value, "()" + field.classType.value)
  }

  def methodToIR(symbolTable: SymbolTable, method: Method, isObject: Boolean): MethodIR = {
    val innerSymbolTable = symbolTable.getInnerSymbolTable()
    val instructions = blockToIR(innerSymbolTable, method.body)

    val isMainMethod = method.name.value.equals("main")

    println("Symbol table")
    println(innerSymbolTable)

    val modifiers = if (!isMainMethod) {
      method.modifiers.map {
        case Private => PrivateIR
        case Protected => ProtectedIR
      }.toList ++ (if (!method.modifiers.exists(modifier => modifier.equals(Public))) List(PublicIR) else List())
    } else {
      List(PublicIR, StaticIR)
    }

    val parameters = if (!isMainMethod) {
      method.parameters.map(parameter => {
        IRUtils.getTypeDescriptor(symbolTable, parameter.classType.value) match {
          case Some(value) => ParameterIR(List(), parameter.name.value, value)
          case None => throw new Exception(s"Type descriptor not found: ${parameter.classType.value}")
        }
      }).toList
    } else {
      List(ParameterIR(List(), "args", "[Ljava/lang/String;"))
    }

    val returnTypeClass = method.returnType.value

    val returnTypeDescriptor =  IRUtils.getTypeDescriptor(symbolTable, returnTypeClass) match {
      case Some(returnType) => returnType
      case None => throw new Exception(s"No method return type: ${returnTypeClass}")
    }

    MethodIR(modifiers, method.name.value, returnTypeDescriptor, parameters, instructions)
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
      case ABinary(op, expr1, expr2) => expressionToIR(symbolTable, expr1) ++ expressionToIR(symbolTable, expr2) :+ opToIR(symbolTable, op)
      case methodCall: MethodCall => methodCallToIR(symbolTable, methodCall, true)
      case nestedExpr: NestedExpr =>nestedExprToIR(symbolTable, nestedExpr)
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

  def methodCallToIR(symbolTable: SymbolTable, methodCall: MethodCall, isFirst: Boolean): List[InstructionIR] ={

    symbolTable.getByIdentifier(methodCall.name.value) match {
      case Some(row) => row match {
        case MethodStructure(name, signature) => {
          val methodSignature: String =  IRUtils.getInternalName(symbolTable, signature).get

          if(isFirst){
            List(ALoad(0),  InvokeVirtual("ClassName",methodCall.name.value,methodSignature))
          } else {
            List(InvokeVirtual("ClassName",methodCall.name.value, methodSignature))
          }
        }
        case _ => throw new Exception(methodCall.toString)
      }
      case None => throw new Exception(methodCall.toString)
    }


  }

  def nestedExprToIR(symbolTable: SymbolTable, nestedExpr: NestedExpr):  List[InstructionIR]  ={
    // Get the first element
    // If is a method call then push the object onto the stack

    nestedExpr.expressions.head match {
      case methodCall: MethodCall => methodCallToIR(symbolTable, methodCall, true) ++ nestedExpr.expressions.drop(1).map(expressionToIR(symbolTable, _)).toList.asInstanceOf[List[InstructionIR]]
      case _ => nestedExpr.expressions.map(expressionToIR(symbolTable, _)).toList.asInstanceOf[List[InstructionIR]]
    }
  }

  def statementToIR(symbolTable: SymbolTable, statement: Statement): List[InstructionIR] = {

    statement match {
      case assign: Assign => assignmentToIR(symbolTable, assign)
      case exprAsStmt: ExprAsStmt => expressionToIR(symbolTable, exprAsStmt.expression)
    }
  }

  def assignmentToIR(symbolTable: SymbolTable, assign: Assign): List[InstructionIR] = {
    val identifier = symbolTable.getByIdentifier(assign.name.value)

    if (identifier.isEmpty) {
      throw new Exception(s"Identifier ${assign.name.value} not found in symbol table: $symbolTable")
    } else {
      val instructions: List[InstructionIR] = blockToIR(symbolTable, assign.block) :+ IStore(identifier.get.id)
      instructions
    }
  }
}
