package compiler.codegen

import java.nio.file.Paths

import compiler.ast.AST._
import compiler.ast2ir.AST2IR
import compiler.ir.IR._
import javassist.bytecode.MethodInfo

object CodeGenTester {

  def main(args: Array[String]): Unit = {

    val module = Module(ModuleHeader(NameSpace(List(Name("x"), Name("y"), Name("z"))),List()),List(ClassModel(Name("ClassName"),List(),List(),None,List(),List(),List(Method(Name("methodName"),List(),List(),List(),None,DoBlock(List(Assign(Name("x"),None,true,Inline(IntConst(10))))))))))

    val compilationUnitIR = AST2IR.moduleToIR(module)

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
