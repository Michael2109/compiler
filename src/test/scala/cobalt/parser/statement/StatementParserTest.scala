package cobalt.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StatementParserTest extends AnyFunSpec with Matchers {
   describe("Statement parser") {
     it("Should parse statements") {
      /* val code =
         """if true then x else y
           """.stripMargin.replace("\r", "")
       TestUtil.parse(code, ExpressionParser.expressionParser(_)) shouldBe Ternary(Identifier(Name("true")), Identifier(Name("x")), Identifier(Name("y")))*/
       println()
     }
   }
}
