package compiler.parser.expression

import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import compiler.parser.ExpressionParser


class ExpressionParserTest extends AnyFunSpec with Matchers {
  describe("Nested expression call parser test") {
    it("Should parse nested expression calls") {

     // TestUtil.parse("x.toString()", ExpressionParser.expressionParser(_)) shouldBe NestedExpr(List(Identifier(Name("x")), MethodCall(Name("toString"), List(BlockExpr(List[Expression]())))))
    }
  }
}