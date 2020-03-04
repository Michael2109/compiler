package compiler.ast2ir

import java.nio.file.Paths

import compiler.ast.AST._
import compiler.codegen.{CodeGen, CodeGenTestUtils}
import compiler.ir.IR._
import compiler.symboltable.{SymbolTable, SymbolTableCreator}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MethodAST2IRTest extends AnyFunSpec with Matchers {
  describe("Method AST to IR") {

    it("Should convert a static method to IR") {

      val method: Method = Method(Name("methodName"), List(), List(), List(), Type("Unit"), DoBlock(List(Assign(Name("x"), None, true, Inline(IntConst(10))))))
      val symbolTable = new SymbolTable

      SymbolTableCreator.genSymbolTable(symbolTable, method)
      val ir = AST2IR.methodToIR(symbolTable, method)
      ir shouldBe MethodIR(List(PublicIR),"methodName","V",List(),List(IConst0(10), IStore(1)))
    }

    it("Should convert a method with parameters"){
      val method: Method = Method(Name("methodName"), List(), List(Parameter(Name("x"), Type("Int"), None)), List(), Type("Unit"), DoBlock(List(Assign(Name("x"), None, true, Inline(IntConst(10))))))
      val symbolTable = new SymbolTable

      SymbolTableCreator.genSymbolTable(symbolTable, method)
      val ir = AST2IR.methodToIR(symbolTable, method)
      ir shouldBe MethodIR(List(PublicIR),"methodName","V",List(ParameterIR(List(),"x","I")),List(IConst0(10), IStore(1)))
    }
  }

}
