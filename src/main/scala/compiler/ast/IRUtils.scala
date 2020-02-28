package compiler.ast

import com.sun.xml.internal.ws.org.objectweb.asm.Opcodes
import compiler.ast.AST._

object IRUtils {
/*
  def typeStringToTypeIR(t: String): TypeIR = {
    t match {
      case "Int" => IntType()
      case "Long" => LongType()
      case "String" => StringLiteralType()
      case "Unit" => UnitType()
      case className => ObjectType(className)
    }
  }*/
/*
  def typeToBytecodeType(`type`: Type): String = {
    `type` match {
      case "int" => "I"
      case "long" => "J"
      case "String" => "Ljava/lang/String;"
      case "Unit" => "V"
      case x => "L" + x+ ";"
    }
  }*/

/*
  def getStoreOperator(statement: Statement): Int = {
    statement match {
      case inline: Inline => getStoreOperator(inline.expression)
      case doBlock: DoBlock => {
        getStoreOperator(doBlock.statement.head)
      }
      case blockStmt: BlockStmt => getStoreOperator(blockStmt.statements.head)
    }
  }

  def getStoreOperator(expression: Expression): Int = {
    expression match {
      case aBinaryIR: ABinary => getStoreOperator(aBinaryIR.expression1)
      case _: IntConstIR => Opcodes.ISTORE
      case _: LongConst => Opcodes.LSTORE
      case _: FloatConst => Opcodes.FSTORE
      case _: DoubleConst => Opcodes.DSTORE
    }
  }

  def getStoreOperator(t: TypeIR, id: Int): StoreOperators = {
    t match {
      case _: IntType => IStore(id)
      case _: LongType => LStore(id)
      case _: StringLiteralType => AStore(id);
      case _: ObjectType => AStore(id)
    }
  }

  def getLoadOperator(t: TypeIR): Int = {
    t match {
      case intType: IntType => Opcodes.ILOAD
      case longType: LongType => Opcodes.LLOAD
      case _ => Opcodes.ALOAD
    }
  }


  def getArithmeticOperator(op: Operator, expression1: Expression, expression2: Expression): Int = {

    expression1 match {
      case innerABinary: ABinary => {
        getArithmeticOperator(op, innerABinary.expression1, innerABinary.expression2)
      }
      case _: IntConstIR => {
        op match {
          case Add => Opcodes.IADD
          case Subtract => Opcodes.ISUB
          case Multiply => Opcodes.IMUL
          case Divide => Opcodes.IDIV
        }
      }
      case _: LongConstIR => {
        op match {
          case Add => Opcodes.LADD
          case Subtract => Opcodes.LSUB
          case Multiply => Opcodes.LMUL
          case Divide => Opcodes.LDIV
        }
      }
      case _: FloatConstIR => {
        op match {
          case Add => Opcodes.FADD
          case Subtract => Opcodes.FSUB
          case Multiply => Opcodes.FMUL
          case Divide => Opcodes.FDIV
        }
      }
      case _: DoubleConstIR => {
        op match {
          case Add => Opcodes.DADD
          case Subtract => Opcodes.DSUB
          case Multiply => Opcodes.DMUL
          case Divide => Opcodes.DDIV
        }
      }
    }
  }*/

  def modifierToOpcode(modifier: Modifier): Int = {
    modifier match {
      case Public => Opcodes.ACC_PUBLIC
      case Protected => Opcodes.ACC_PROTECTED
      case Private => Opcodes.ACC_PRIVATE
      case Abstract => Opcodes.ACC_ABSTRACT
      case Final => Opcodes.ACC_FINAL
    }
  }
}
