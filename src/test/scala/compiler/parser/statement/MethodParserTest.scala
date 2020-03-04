package compiler.parser.statement

import compiler.ast.AST._
import compiler.parser.StatementParser
import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MethodParserTest extends AnyFunSpec with Matchers {
  describe("Statement parser") {
    it("Should parse a method with no parameters") {
      val code =
        """let methodName() = do
          |    let x = 10
            """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.methodParser(_)) shouldBe Method(Name("methodName"), List(), List(), List(), Type("Unit"), DoBlock(List(Assign(Name("x"), None, true, Inline(IntConst(10))))))
    }

    it("Should parse a method with one parameter") {
      val code =
        """let methodName(x: Int) = do
          |    let x = 10
            """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.methodParser(_)) shouldBe Method(Name("methodName"),List(),List(Parameter(Name("x"),Type("Int"),None)),List(),Type("Unit"),DoBlock(List(Assign(Name("x"),None,true,Inline(IntConst(10))))))
    }

  }
}
