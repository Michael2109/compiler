package compiler.parser.expression

import compiler.ast.AST.{ABinary, Add, Divide, IntConst, Multiply, Subtract}
import compiler.parser.ExpressionParser
import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ArithmeticExpressionParserTest  extends AnyFunSpec with Matchers{
  describe("Arithmetic expression parser test") {
    it("Should parse addition") {
      TestUtil.parse("14 + 105", ExpressionParser.arithmeticExpressionParser(_)) shouldBe ABinary(Add,IntConst(14),IntConst(105))
    }
    it("Should parse subtraction") {
      TestUtil.parse("81 - 9", ExpressionParser.arithmeticExpressionParser(_)) shouldBe ABinary(Subtract,IntConst(81),IntConst(9))
    }
    it("Should parse multiplication") {
      TestUtil.parse("44 * 8", ExpressionParser.arithmeticExpressionParser(_)) shouldBe ABinary(Multiply,IntConst(44),IntConst(8))
    }
    it("Should parse division") {
      TestUtil.parse("5 / 76", ExpressionParser.arithmeticExpressionParser(_)) shouldBe ABinary(Divide,IntConst(5),IntConst(76))
    }
    it("Should parse parenthesis") {
      TestUtil.parse("(5 + 76)", ExpressionParser.arithmeticExpressionParser(_)) shouldBe ABinary(Add,IntConst(5),IntConst(76))
    }
    it("Should parse multiple operators") {
      TestUtil.parse("8 * (5 + 76) / 27 - 6", ExpressionParser.arithmeticExpressionParser(_)) shouldBe
        ABinary(Subtract,ABinary(Divide,ABinary(Multiply,IntConst(8),ABinary(Add,IntConst(5),IntConst(76))),IntConst(27)),IntConst(6))
    }
  }
}
