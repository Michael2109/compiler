package compiler.parser.statement

import compiler.ast.AST
import compiler.ast.AST.{Assign, ClassModel, DoBlock, Identifier, Inline, IntConst, Method, Module, ModuleHeader, Name, NameSpace, RefLocal, Type}
import compiler.parser.StatementParser
import compiler.utils.TestUtil
import javassist.bytecode.stackmap.TypeData.ClassName
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StatementParserTest extends AnyFunSpec with Matchers {
  describe("Statement parser") {
    it("Should parse statements") {
        val code =
          """package x.y.z
            |class ClassName
            |    let methodName() = do
            |        let x = 10
            """.stripMargin.replace("\r", "")
        TestUtil.parse(code, StatementParser.moduleParser(_)) shouldBe Module(ModuleHeader(NameSpace(List(Name("x"), Name("y"), Name("z"))),List()),List(ClassModel(Name("ClassName"),List(),List(),None,List(),List(),List(Method(Name("methodName"),List(),List(),List(),None,DoBlock(List(Assign(Name("x"),None,true,Inline(IntConst(10))))))))))
        println()
      }
  }
}
