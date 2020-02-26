package compiler.parser.expression

import compiler.ast.AST.IntConst
import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import compiler.parser.ExpressionParser


class ExpressionParserTest extends AnyFunSpec with Matchers {
  describe("Nested expression call parser test") {
    it("Should parse nested expression calls") {

      TestUtil.parse("1", ExpressionParser.allExpressionsParser(_)) shouldBe IntConst(1)
    }
  }
}
