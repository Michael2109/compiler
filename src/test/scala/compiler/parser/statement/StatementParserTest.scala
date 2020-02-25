package compiler.parser.statement

import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import compiler.ast.AST.identifier
import compiler.parser.StatementParser

class StatementParserTest extends AnyFunSpec with Matchers {
   describe("Statement parser") {
     it("Should parse statements") {
       val code =
         """x = 10
           """.stripMargin.replace("\r", "")
       TestUtil.parse(code, StatementParser.file_input(_)) shouldBe identifier("")
       println()
     }
   }
}