package compiler.parser.expression

import compiler.ast.AST.{ABinary, Add, IntConst}
import compiler.parser.ExpressionParser
import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


class ExpressionParserTest extends AnyFunSpec with Matchers {
  describe("Nested expression call parser test") {
    it("Should parse nested expression calls") {

      TestUtil.parse("1+1", ExpressionParser.expressionParser(_)) shouldBe ABinary(Add, IntConst(1), IntConst(1))
    }
  }
}
