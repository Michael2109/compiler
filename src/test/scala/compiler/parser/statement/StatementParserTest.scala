package compiler.parser.statement

import compiler.ast.AST.Identifier
import compiler.parser.StatementParser
import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StatementParserTest extends AnyFunSpec with Matchers {
  describe("Statement parser") {
    it("Should parse statements") {
        val code =
          """class ClassName
            |      let value: Int = 10
            """.stripMargin.replace("\r", "")
        TestUtil.parse(code, StatementParser.statementParser(_)) shouldBe ""
        println()
      }
  }
}
