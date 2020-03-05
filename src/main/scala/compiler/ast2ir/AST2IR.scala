package compiler.ast2ir

import compiler.ast.AST.{Add, _}
import compiler.ir.IR._
import compiler.symboltable.SymbolTable

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

    val modelInternalName = getInternalName(symbolTable, model.name.value).get

    val modelTypeDescriptor = getTypeDescriptor(symbolTable, model.name.value).get

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
        getTypeDescriptor(symbolTable, parameter.classType.value) match {
          case Some(value) => ParameterIR(List(), parameter.name.value, value)
          case None => throw new Exception(s"Type descriptor not found: ${parameter.classType.value}")
        }
      }).toList
    } else {
      List(ParameterIR(List(), "args", "[Ljava/lang/String;"))
    }

    val returnTypeClass = method.returnType.value

    val returnTypeDescriptor = getTypeDescriptor(symbolTable, returnTypeClass) match {
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
      case methodCall: MethodCall => methodCallToIR(symbolTable, methodCall)
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

  def methodCallToIR(symbolTable: SymbolTable, call: MethodCall): MethodCallIR ={

  }

  def statementToIR(symbolTable: SymbolTable, statement: Statement): List[InstructionIR] = {

    statement match {
      case assign: Assign => assignmentToIR(symbolTable, assign)
      case exprAsStmt: ExprAsStmt => expressionToIR(exprAsStmt.expression)
    }
  }

  def assignmentToIR(symbolTable: SymbolTable, assign: Assign): List[InstructionIR] = {
    val identifier = symbolTable.findIdentifier(assign.name.value)

    if (identifier.isEmpty) {
      throw new Exception(s"Identifier ${assign.name.value} not found in symbol table: $symbolTable")
    } else {
      val instructions: List[InstructionIR] = blockToIR(symbolTable, assign.block) :+ IStore(identifier.get.id)
      instructions
    }
  }

  /**
   * Example: com/snark/Boojum
   *
   * @param symbolTable
   * @param className
   * @return
   */
  def getInternalName(symbolTable: SymbolTable, className: String): Option[String] = {
    symbolTable.getImport(className) match {
      case Some(locations) => Some(locations.mkString("/"))
      case None => None
    }
  }

  /**
   * Example: [[Ljava/lang/Object;
   *
   * @param symbolTable
   * @param className
   * @return
   */
  def getTypeDescriptor(symbolTable: SymbolTable, className: String): Option[String] = {
    getInternalName(symbolTable, className) match {
      case Some(internalName) => Some(s"L$internalName;")
      case None => className match {
        case "Unit" => Some("V")
        case "Int" => Some("I")
        case _ => None
      }
    }
  }

}
