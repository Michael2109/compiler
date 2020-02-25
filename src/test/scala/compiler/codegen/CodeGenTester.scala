package compiler.codegen

import java.nio.file.Paths

import compiler.ast.IR.{ClassIR, MethodIR}
import javassist.bytecode.MethodInfo

object CodeGenTester {

  def main(args: Array[String]): Unit = {

    val constructor = MethodIR(List(), MethodInfo.nameInit, "V", List(), List())
    val method1 = MethodIR(List(), "methodName", "V", List(), List())

    val methods = List(constructor, method1)
    val classIR = ClassIR(List(), "ClassName", "V", List(), List(), methods)

    CodeGenTestUtils.writeClassToFile(CodeGen.genClassIRCode(classIR), Paths.get("test_file.class"))
  }

}
