package compiler.ast2ir

import compiler.ast.AST.{ABinary, Add, Divide, IntConst, Multiply, Subtract}
import compiler.ir.IR.{IAdd, IConst0, IDivide, IMultiply, ISubtract}
import compiler.parser.ExpressionParser
import compiler.symboltable.SymbolTable
import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ArithmeticAST2IRTest  extends AnyFunSpec with Matchers{
  describe("Arithmetic AST to IR") {
    val symbolTable = new SymbolTable

    it("Should convert addition") {
      val ir = AST2IR.expressionToIR(symbolTable,ABinary(Add, IntConst(14), IntConst(105)))
      ir shouldBe List(IConst0(14), IConst0(105), IAdd)
    }
    it("Should convert subtraction") {
      val ir = AST2IR.expressionToIR(symbolTable,ABinary(Subtract, IntConst(14), IntConst(105)))
      ir shouldBe List(IConst0(14), IConst0(105), ISubtract)
    }
    it("Should convert multiplication") {
      val ir = AST2IR.expressionToIR(symbolTable,ABinary(Multiply, IntConst(14), IntConst(105)))
      ir shouldBe List(IConst0(14), IConst0(105), IMultiply)
    }
    it("Should convert division") {
      val ir = AST2IR.expressionToIR(symbolTable,ABinary(Divide, IntConst(14), IntConst(105)))
      ir shouldBe List(IConst0(14), IConst0(105), IDivide)
    }
    it("Should convert multiple arithmetic operations") {
      val ir = AST2IR.expressionToIR(symbolTable,ABinary(Add, IntConst(50), ABinary(Divide, IntConst(14), IntConst(105))))
      ir shouldBe List(IConst0(50), IConst0(14), IConst0(105), IDivide, IAdd)
    }
  }
}
