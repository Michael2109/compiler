package compiler.ast2ir

import compiler.ast.AST._
import compiler.ir.IR._
import compiler.symboltable.SymbolTable
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ArithmeticAST2IRTest extends AnyFunSpec with Matchers {
  describe("Arithmetic AST to IR") {
    val symbolTable = new SymbolTable(None)

    it("Should convert addition") {
      val ir = AST2IR.expressionToIR(symbolTable, ABinary(Add, IntConst(14), IntConst(105)))
      ir shouldBe List(IConst0(14), IConst0(105), IAdd)
    }
    it("Should convert subtraction") {
      val ir = AST2IR.expressionToIR(symbolTable, ABinary(Subtract, IntConst(14), IntConst(105)))
      ir shouldBe List(IConst0(14), IConst0(105), ISubtract)
    }
    it("Should convert multiplication") {
      val ir = AST2IR.expressionToIR(symbolTable, ABinary(Multiply, IntConst(14), IntConst(105)))
      ir shouldBe List(IConst0(14), IConst0(105), IMultiply)
    }
    it("Should convert division") {
      val ir = AST2IR.expressionToIR(symbolTable, ABinary(Divide, IntConst(14), IntConst(105)))
      ir shouldBe List(IConst0(14), IConst0(105), IDivide)
    }
    it("Should convert multiple arithmetic operations") {
      val ir = AST2IR.expressionToIR(symbolTable, ABinary(Add, IntConst(50), ABinary(Divide, IntConst(14), IntConst(105))))
      ir shouldBe List(IConst0(50), IConst0(14), IConst0(105), IDivide, IAdd)
    }
  }
}
