package compiler.utils

import java.io.File
import java.nio.file.{Path, Paths}

import compiler.compiler.CompilerExecutor

import scala.language.postfixOps
import scala.sys.process._

object CompilerUtil {

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def compileDirectory(classPath: Path, outputDir: Path, currentDir: Path): Unit = {
    val allFiles = recursiveListFiles(classPath.resolve(currentDir).toFile).filter(!_.isDirectory)

    for (file <- allFiles) {
      CompilerExecutor.main(Array("-cp", classPath.toString, "-d", outputDir.toString, classPath.relativize(file.toPath).toString));
    }
  }

  def deleteFile(classPath: Path, outputDir: Path, filePath: Path): Unit = {
    outputDir.resolve(filePath).toFile.delete()
  }

  def compileFile(classPath: Path, outputDir: Path, filePath: Path): Unit = {
    outputDir.resolve(filePath).getParent.toFile.mkdirs
    CompilerExecutor.main(Array("-cp", classPath.toString, "-d", outputDir.toString, filePath.toString + ".compiler"))
  }

  //compileDirectory(Paths.get("src\\test\\resources\\compiler"), Paths.get("cobalt_generated"), Paths.get(""))

  def executeJava(fileName: Path): Array[String] = {
    compileFile(Paths.get("src/test/resources/compiler"), Paths.get("cobalt_generated"), fileName)
    compileFile(Paths.get("src/test/resources/compiler"), Paths.get("cobalt_generated"), fileName)

    val result: String = (("java -cp " + Paths.get("cobalt_generated").toString + " " + fileName.toString.replace("\\", ".")) !!)
    result.split("\r\n")
  }
}
