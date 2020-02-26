package compiler.compiler

import java.nio.file.{Path, Paths}

import fastparse._
import compiler.ast.AST
import compiler.parser.StatementParser

import scala.io.Source

object Compiler {

  def compile(commandLineOptions: Map[CommandLineOption, Any], classPath: Path, pathsToCompile: List[Path], outputDir: Path) = {

    val absolutePathsToCompile: List[Path] = pathsToCompile.map(path => Paths.get(classPath.toString, path.toString))

    val cobaltFiles: List[String] = absolutePathsToCompile.map(x => Source.fromFile(x.toFile).mkString)

    // Parse them
    val asts = cobaltFiles.map(cobaltFile => {
      //parse(cobaltFile.replace("\r", ""), StatementParser.file_input(_))
    })
/*

    val modules: Seq[AST.Module] = asts.map(ast => ast match {
      case Parsed.Success(value, _) => value
      case Parsed.Failure(a, b, c) => throw new Exception("Failed compiling: " + a + " : " + b + " : " + c)
    })
*/



    /*
        // Process AST
        val modelIRs: Seq[ModelIR] = modules.map(x => AST2IR.astToIR(x)).head

        // Generate code
        val moduleBytecodes: Seq[Array[Byte]] = modelIRs.map(CodeGen.genModelCode)

        // Save to destination directory
        val generatedFilePath = outputDir.resolve(pathsToCompile(0))

        val filePath = Paths.get(generatedFilePath.toString.replaceFirst("[.][^.]+$", "") + ".class")

        // Create class file
        val file = filePath.toFile
        file.createNewFile()

        val bos = new BufferedOutputStream(new FileOutputStream(filePath.toFile))

        bos.write(moduleBytecodes(0))
        bos.close()*/
  }
}
