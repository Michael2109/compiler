package compiler.compiletester

import java.nio.file.Paths

import compiler.ast.AST.{Assign, DoBlock, Inline, IntConst, Method, Module, ModuleHeader, Name, NameSpace, ObjectModel, Type}
import compiler.ast2ir.AST2IR
import compiler.codegen.{CodeGen, CodeGenTestUtils}
import compiler.parser.StatementParser
import compiler.symboltable.{SymbolTable, SymbolTableCreator}
import compiler.utils.TestUtil

object CompileTester {

  def main(args: Array[String]): Unit = {
    val code =
      """package x.y.z
        |object ClassName
        |    let main(test: Int): Unit = do
        |        let x = 200
        |        let y = 200 * 2
            """.stripMargin.replace("\r", "")
    val module: Module = TestUtil.parse(code, StatementParser.moduleParser(_)).asInstanceOf[Module]


    val symbolTable =  SymbolTableCreator.genSymbolTable(module)
    val compilationUnitIR = AST2IR.moduleToIR(symbolTable, module)

    compilationUnitIR.classes.foreach(clazz => {

      val bytecode = CodeGen.genModelIRCode(clazz)
      CodeGenTestUtils.writeClassToFile(bytecode, Paths.get(clazz.name + ".class"))
    })
  }

}
