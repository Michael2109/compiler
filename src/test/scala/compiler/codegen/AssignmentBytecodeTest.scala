package compiler.codegen

import compiler.ast.AST.Module
import compiler.ast2ir.AST2IR
import compiler.parser.StatementParser
import compiler.symboltable.SymbolTableCreator
import compiler.utils.TestUtil
import javassist.bytecode.Bytecode
import javassist.{ByteArrayClassPath, ClassPool, CtClass}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AssignmentBytecodeTest extends AnyFunSpec with Matchers {
  describe("Generate bytecode for assignments") {
    it("Should generate bytecode for one variable") {

      val code =
        """package x.y.z
          |class ClassName
          |    let methodName(): Unit = do
          |        let x = 10 + 10
            """.stripMargin.replace("\r", "")
      val module = TestUtil.parse(code, StatementParser.moduleParser(_)).asInstanceOf[Module]

      val symbolTable = SymbolTableCreator.genSymbolTable(module)

      val compilationUnitIR = AST2IR.moduleToIR(symbolTable, module)


      compilationUnitIR.classes.foreach(clazz => {
        val bytecode = CodeGen.genModelIRCode(clazz)

        val cp = ClassPool.getDefault();
        cp.insertClassPath(new ByteArrayClassPath(clazz.name, bytecode));
        val cc: CtClass = cp.get(clazz.name);

        val method = cc.getDeclaredMethod("methodName")

       // val bytecpde = new Bytecode(cp)

        println(method.getMethodInfo2.getCodeAttribute.getCode)
      })
    }
  }

}
