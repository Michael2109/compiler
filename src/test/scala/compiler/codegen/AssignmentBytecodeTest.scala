package compiler.codegen

import java.nio.file.Paths

import compiler.ast.AST.Module
import compiler.ast2ir.AST2IR
import compiler.parser.StatementParser
import compiler.symboltable.SymbolTableCreator
import compiler.utils.TestUtil
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AssignmentBytecodeTest extends AnyFunSpec with Matchers {
  describe("Generate bytecode for assignments") {
    it("Should generate bytecode for one variable") {

      val code =
        """package x.y.z
          |class ClassName
          |    let methodName() = do
          |        let x = (10 + 20 - 30) / 50 * 70
          |
          |        let y = 100 - 50
          |
          |        let z = 2000 / 405 + 20
            """.stripMargin.replace("\r", "")
      val module = TestUtil.parse(code, StatementParser.moduleParser(_)).asInstanceOf[Module]

      val symbolTable = SymbolTableCreator.genSymbolTable(module)

      val compilationUnitIR = AST2IR.moduleToIR(symbolTable,module)

      compilationUnitIR.classes.foreach(clazz => {
        CodeGenTestUtils.writeClassToFile(CodeGen.genClassIRCode(clazz), Paths.get(s"${clazz.name}.class"))
      })
    }
  }

}
