package compiler.parser

import compiler.parser.ExpressionParser.{whitespace => _}

object StatementParser extends Statements(0)

/**
 * Python's statement grammar. This can only be used in statement-blocks,
 * and is sensitive to newlines and indentation to determine nesting
 *
 * Manually transcribed from https://docs.python.org/2/reference/grammar.html
 */
class Statements(indent: Int) {
  /*
    implicit def whitespace(cfg: P[_]): P[Unit] = LexicalParser.wscomment(cfg)

    def space[_: P] = P(CharIn(" \n"))

    def NEWLINE[_: P]: P0 = P("\n" | End)

    def ENDMARKER[_: P]: P0 = P(End)

    def indents[_: P] = P("\n" ~~ " ".repX(indent))

    def spaces[_: P] = P((LexicalParser.nonewlinewscomment.? ~~ "\n").repX(1))

    def assignParser[_: P]: P[Assign] = P(LexicalParser.kw("let") ~ ("mutable").!.? ~ ExpressionParser.nameParser ~ (":" ~ ExpressionParser.typeRefParser).? ~/ P(LexicalParser.kw("=")) ~ blockParser).map(x => Assign(x._2, x._3, x._1.isEmpty, x._4))

    def blockParser: P[Block] = P(doBlock | ExpressionParser.expressionParser.map(Inline))

    def commentParser: P[_] = P(LexicalParser.comment)

    def doBlock: P[Block] = P(LexicalParser.kw("do") ~~ indentedBlock).map(x => DoBlock(x))

    def exprAsStmt: P[Statement] = P(ExpressionParser.expressionParser).map(ExprAsStmt)

    def ifStatementParser: P[If] = {
      def ifParser: P[(Expression, Statement)] = P(LexicalParser.kw("if") ~/ ExpressionParser.expressionParser ~ P(LexicalParser.kw("then")) ~ blockParser).map(x => (x._1, x._2))
      def elseParser: P[Statement] = P(elifP ~ elseParser.?).map(x => If(x._1, x._2, x._3)) | P(elseP)

      def elifP: P[(Expression, Statement)] = P(LexicalParser.kw("elif") ~/ ExpressionParser.expressionParser ~ LexicalParser.kw("then") ~ blockParser).map(x => (x._1, x._2))
      def elseP: P[Statement] = P(LexicalParser.kw("else") ~/ blockParser).map(x => x)

      P(ifParser ~ elseParser.?).map(x => If(x._1, x._2, x._3))
    }

    def importParser: P[Import] = P(LexicalParser.kw("import") ~/ ExpressionParser.nameParser.rep(sep=".")).map(Import)

    def fieldParser: P[Field] = P(ExpressionParser.nameParser ~ ":" ~ ExpressionParser.typeRefParser).map(x => Field(x._1, x._2, None))

    def methodParser: P[Statement] = P(ExpressionParser.modifiers ~ LexicalParser.kw("let") ~ ExpressionParser.nameParser ~ "(" ~/ fieldParser.rep(sep = ",") ~ ")" ~ (":" ~ ExpressionParser.typeRefParser).? ~ "=" ~ blockParser).map(x => Method(x._2, Seq(), x._3, x._1, x._4, x._5))

    def modelParser: P[Model] = P(LexicalParser.kw("class") ~/ ExpressionParser.nameParser ~ ("extends" ~ ExpressionParser.typeRefParser).? ~ (LexicalParser.kw("with") ~ ExpressionParser.typeRefParser).rep() ~~ indentedBlock).map(x => ClassModel(x._1, Seq(), Seq(), x._2, Seq(), x._3, x._4))

    def moduleParser: P[Module] = P(nameSpaceParser ~ importParser.rep ~ modelParser.rep).map(x => Module(ModuleHeader(x._1, x._2), x._3))

    def nameSpaceParser: P[NameSpace] = P(LexicalParser.kw("package") ~/ ExpressionParser.nameParser.rep(sep=".")).map(NameSpace)

    def reassignParser: P[Reassign] = P(ExpressionParser.nameParser ~ "<-" ~/ blockParser).map(x => Reassign(x._1, x._2))

    def statementParser: P[Statement] = P(!commentParser ~ (modelParser | ifStatementParser | methodParser | assignParser | reassignParser | exprAsStmt))

    def suite[_: P]: P[Seq[AST.stmt]] = {
      def deeper: P[Int] = {
        def commentLine = P("\n" ~~ LexicalParser.nonewlinewscomment.?.map(_ => 0)).map((_, Some("")))

        def endLine = P("\n" ~~ (" " | "\t").repX(indent + 1).!.map(_.length) ~~ LexicalParser.comment.!.?)

        P(LexicalParser.nonewlinewscomment.? ~~ (endLine | commentLine).repX(1)).map {
          _.collectFirst { case (s, None) => s }
        }.filter(_.isDefined).map(_.get)
      }

      def indented = P(deeper.flatMapX { nextIndent =>
        new Statements(nextIndent).stmt.repX(1, spaces.repX(1) ~~ (" " * nextIndent | "\t" * nextIndent)).map(_.flatten)
      })

      P(indented | " ".rep ~ simple_stmt)
    }*/
}
