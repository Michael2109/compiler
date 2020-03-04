package compiler.ast2ir

import java.nio.file.Paths

import compiler.ast.AST.{ABinary, Add, Assign, ClassModel, DoBlock, Inline, IntConst, Method, Model, Module, ModuleHeader, Name, NameSpace, ObjectModel, Type}
import compiler.codegen.{CodeGen, CodeGenTestUtils}
import compiler.ir.IR.{ALoad, ClassModelTypeIR, Dup, FieldIR, FinalIR, IAdd, IConst0, IStore, InvokeSpecial, MethodIR, ModelIR, New, ObjectModelTypeIR, PrivateIR, PublicIR, PutStatic, ReturnIR, StaticIR}
import compiler.symboltable.{SymbolTable, SymbolTableCreator}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ModelAST2IRTest extends AnyFunSpec with Matchers {
  describe("Class model AST to IR") {

    it("Should convert class model to IR") {

      val model: Model = ClassModel(Name("ClassName"), List(), List(), None, List(), List(), List(Method(Name("methodName"), List(), List(), List(), Type("Unit"), DoBlock(List(Assign(Name("x"), None, true, Inline(IntConst(10))))))))
      val symbolTable = new SymbolTable

      SymbolTableCreator.genSymbolTable(symbolTable, model)
      val ir = AST2IR.modelToIR(symbolTable, model)
      ir shouldBe ModelIR(ClassModelTypeIR,List(),"ClassName",None,List(),List(),List(MethodIR(List(PublicIR),"methodName","V",List(),List(IConst0(10), IStore(1)))))
    }
  }

  describe("Object model AST to IR") {

    it("Should convert object model to IR") {

      val model: Model = ObjectModel(Name("ClassName"), List(), List(), None, List(), List(), List(Method(Name("methodName"), List(), List(), List(), Type("Unit"), DoBlock(List(Assign(Name("x"), None, true, Inline(IntConst(10))))))))
      val symbolTable = new SymbolTable

      SymbolTableCreator.genSymbolTable(symbolTable, model)
      val ir = AST2IR.modelToIR(symbolTable, model)

      val bytecode = CodeGen.genModelIRCode(ir)
      CodeGenTestUtils.writeClassToFile(bytecode, Paths.get(ir.name + ".class"))
      ir shouldBe ModelIR(ObjectModelTypeIR,List(),"ClassName",None,List(),List(FieldIR(List(FinalIR, PrivateIR),"instance","ClassName")),List(MethodIR(List(PublicIR, StaticIR),"<clinit>","V",List(),List(New("ClassName"), Dup, InvokeSpecial("ClassName","<init>","()V"), PutStatic("ClassName","instance","ClassName"), ReturnIR)), MethodIR(List(PublicIR),"<init>","V",List(),List(ALoad(0), InvokeSpecial("java/lang/Object","<init>","()V"), ReturnIR)), MethodIR(List(PublicIR),"methodName","V",List(),List(IConst0(10), IStore(1)))))
    }
  }
}
