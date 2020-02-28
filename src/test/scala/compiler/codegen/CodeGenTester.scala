package compiler.codegen

import java.nio.file.Paths

import compiler.ast.AST._
import compiler.ast2ir.AST2IR
import compiler.ir.IR._
import compiler.parser.StatementParser
import compiler.symboltable.SymbolTableCreator
import compiler.utils.TestUtil
import javassist.bytecode.MethodInfo

object CodeGenTester {

  def main(args: Array[String]): Unit = {

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

    println(symbolTable)

    val compilationUnitIR = AST2IR.moduleToIR(symbolTable,module)

    println(compilationUnitIR)

    val constructor = MethodIR(List(), MethodInfo.nameInit, "V", List(), List(ALoad(0), InvokeSpecial("java/lang/Object", MethodInfo.nameInit, "()V"), compiler.ir.IR.ReturnIR(), MaxLocals(1)))
    val method1 = MethodIR(List(), "methodName", "V", List(), List(ReturnIR()))

    val methods = List(constructor, method1)
    val classIR = ClassIR(List(), "ClassName", Some("V"), List(), List(), methods)

    compilationUnitIR.classes.foreach(clazz => {
      CodeGenTestUtils.writeClassToFile(CodeGen.genClassIRCode(clazz), Paths.get(s"${clazz.name}.class"))
    })

  }

}
