package compiler.parser.statement

import compiler.ast.AST._
import compiler.parser.StatementParser
import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ModelParserTest extends AnyFunSpec with Matchers {
  describe("Statement parser") {
    it("Should parse an object") {
      val code =
        """package x.y.z
          |object ClassName
          |    let methodName() = do
          |        let x = 10
            """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.moduleParser(_)) shouldBe Module(ModuleHeader(NameSpace(List(Name("x"), Name("y"), Name("z"))), List()), List(ObjectModel(Name("ClassName"), List(), List(), None, List(), List(), List(Method(Name("methodName"), List(), List(), List(), Type("Unit"), DoBlock(List(Assign(Name("x"), None, true, Inline(IntConst(10))))))))))
    }

    it("Should parse a class") {
      val code =
        """package x.y.z
          |class ClassName
          |    let methodName() = do
          |        let x = 10
            """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.moduleParser(_)) shouldBe Module(ModuleHeader(NameSpace(List(Name("x"), Name("y"), Name("z"))), List()), List(ClassModel(Name("ClassName"), List(), List(), None, List(), List(), List(Method(Name("methodName"), List(), List(), List(), Type("Unit"), DoBlock(List(Assign(Name("x"), None, true, Inline(IntConst(10))))))))))

    }
  }
}
