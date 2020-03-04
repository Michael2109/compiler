package compiler.parser

import compiler.ast.AST._
import compiler.parser.ExpressionParser.{whitespace => _}
import fastparse._

object StatementParser extends Statements(0)

/**
 * Python's statement grammar. This can only be used in statement-blocks,
 * and is sensitive to newlines and indentation to determine nesting
 *
 * Manually transcribed from https://docs.python.org/2/reference/grammar.html
 */
class Statements(indent: Int) {

  implicit def whitespace(cfg: P[_]): P[Unit] = LexicalParser.wscomment(cfg)

  def space[_: P] = P(CharIn(" \n"))

  def NEWLINE[_: P]: P0 = P("\n" | End)

  def ENDMARKER[_: P]: P0 = P(End)

  def indents[_: P] = P("\n" ~~ " ".repX(indent))

  def spaces[_: P] = P((LexicalParser.nonewlinewscomment.? ~~ "\n").repX(1))

  def assignParser[_: P]: P[Assign] = P(LexicalParser.kw("let") ~ ("mutable").!.? ~ ExpressionParser.nameParser ~ (":" ~ ExpressionParser.typeParser).? ~/ P(LexicalParser.kw("=")) ~ blockParser).map(x => Assign(x._2, x._3, x._1.isEmpty, x._4))

  def blockParser[_: P]: P[Block] = P(doBlock | ExpressionParser.expressionParser.map(Inline))

  def commentParser[_: P]: P[_] = P(LexicalParser.comment)

  def doBlock[_: P](): P[Block] = P(LexicalParser.kw("do") ~~ indentedBlock).map(x => DoBlock(x))

  def exprAsStmt[_: P]: P[Statement] = P(ExpressionParser.expressionParser).map(ExprAsStmt)

  def ifParser[_: P]: P[(Expression, Statement)] = P(LexicalParser.kw("if") ~/ ExpressionParser.expressionParser ~ P(LexicalParser.kw("then")) ~ blockParser).map(x => (x._1, x._2))

  def elseParser[_: P]: P[Statement] = P(elifP ~ elseParser.?).map(x => If(x._1, x._2, x._3)) | P(elseP)

  def elifP[_: P]: P[(Expression, Statement)] = P(LexicalParser.kw("elif") ~/ ExpressionParser.expressionParser ~ LexicalParser.kw("then") ~ blockParser).map(x => (x._1, x._2))

  def elseP[_: P]: P[Statement] = P(LexicalParser.kw("else") ~/ blockParser).map(x => x)

  def ifStatementParser[_: P]: P[If] = {

    P(ifParser ~ elseParser.?).map(x => If(x._1, x._2, x._3))
  }

  def importParser[_: P]: P[Import] = P(LexicalParser.kw("import") ~/ ExpressionParser.nameParser.rep(sep = ".")).map(Import)

  def fieldParser[_: P]: P[Field] = P(ExpressionParser.nameParser ~ ":" ~ ExpressionParser.typeParser).map(x => Field(x._1, x._2, None))

  def parameterParser[_: P]: P[Parameter] = P(ExpressionParser.nameParser ~ ":" ~ ExpressionParser.typeParser).map(x => Parameter(x._1, x._2, None))

  def methodParser[_: P]: P[Statement] = P(ExpressionParser.modifiers ~ LexicalParser.kw("let") ~ ExpressionParser.nameParser ~ "(" ~/ parameterParser.rep(sep = ",") ~ ")" ~ (":" ~ ExpressionParser.typeParser) ~ "=" ~ blockParser).map(x => Method(x._2, Seq(), x._3, x._1, x._4, x._5))

  def modelParser[_: P]: P[Model] = P(classParser | objectParser)

  def classParser[_: P]: P[Model] = P(LexicalParser.kw("class") ~/ ExpressionParser.nameParser ~ ("extends" ~ ExpressionParser.typeParser).? ~ (LexicalParser.kw("with") ~ ExpressionParser.typeParser).rep() ~~ indentedBlock).map(x => ClassModel(x._1, Seq(), Seq(), x._2, Seq(), x._3, x._4.toList))

  def objectParser[_: P]: P[Model] = P(LexicalParser.kw("object") ~/ ExpressionParser.nameParser ~ ("extends" ~ ExpressionParser.typeParser).? ~ (LexicalParser.kw("with") ~ ExpressionParser.typeParser).rep() ~~ indentedBlock).map(x => ObjectModel(x._1, Seq(), Seq(), x._2, Seq(), x._3, x._4.toList))

  def moduleParser[_: P]: P[Module] = P(nameSpaceParser ~ importParser.rep ~ modelParser.rep).map(x => Module(ModuleHeader(x._1, x._2), x._3))

  def nameSpaceParser[_: P]: P[NameSpace] = P(LexicalParser.kw("package") ~/ ExpressionParser.nameParser.rep(sep = ".")).map(NameSpace)

  def reassignParser[_: P]: P[Reassign] = P(ExpressionParser.nameParser ~ "<-" ~/ blockParser).map(x => Reassign(x._1, x._2))

  def statementParser[_: P]: P[Statement] = P(!commentParser ~ (modelParser | ifStatementParser | methodParser | assignParser | reassignParser | exprAsStmt))

  def commentLine[_: P] = P("\n" ~~ LexicalParser.nonewlinewscomment.?.map(_ => 0)).map((_, Some("")))

  def endLine[_: P] = P("\n" ~~ (" " | "\t").repX(indent + 1).!.map(_.length) ~~ LexicalParser.comment.!.?)

  def deeper[_: P]: P[Int] = {
    P(LexicalParser.nonewlinewscomment.? ~~ (endLine | commentLine).repX(1)).map {
      _.collectFirst { case (s, None) => s }
    }.filter(_.isDefined).map(_.get)
  }

  def indented[_: P]: P[Seq[Statement]] = P(deeper.flatMap { nextIndent =>
    new Statements(nextIndent).statementParser.repX(1, spaces.repX(1) ~~ (" " * nextIndent | "\t" * nextIndent)).map(x => x)
  })

  def indentedBlock[_: P]: P[Seq[Statement]] = {

    (indented | (" ".rep ~ statementParser.rep(min = 1, max = 1)))
  }
}
