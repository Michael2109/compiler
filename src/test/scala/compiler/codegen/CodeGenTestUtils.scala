package compiler.codegen

import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.file.Path

object CodeGenTestUtils {

  def writeClassToFile(classByteArray: Array[Byte], path: Path): Unit = {
    val target = new BufferedOutputStream(new FileOutputStream(path.toFile))
    try classByteArray.foreach(target.write(_)) finally target.close()
  }
}
