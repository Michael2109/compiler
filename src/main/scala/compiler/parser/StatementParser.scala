package compiler.parser

import fastparse._
import LexicalParser.keyword
import compiler.ast.AST

import fastparse._
import ExpressionParser.{whitespace => _, _}

object StatementParser extends Statements(0)
/**
 * Python's statement grammar. This can only be used in statement-blocks,
 * and is sensitive to newlines and indentation to determine nesting
 *
 * Manually transcribed from https://docs.python.org/2/reference/grammar.html
 */
class Statements(indent: Int){
  implicit def whitespace(cfg: P[_]): P[Unit] = LexicalParser.wscomment(cfg)
  def space[_: P] = P( CharIn(" \n") )
  def NEWLINE[_: P]: P0 = P( "\n" | End )
  def ENDMARKER[_: P]: P0 = P( End )

  def single_input[_: P]: P[Seq[AST.stmt]] = P(
    NEWLINE.map(_ => Nil) |
      simple_stmt |
      compound_stmt.map(Seq(_)) ~ NEWLINE
  )

  def indents[_: P] = P( "\n" ~~ " ".repX(indent) )

  def spaces[_: P] = P( (LexicalParser.nonewlinewscomment.? ~~ "\n").repX(1) )
  def file_input[_: P]: P[Seq[AST.stmt]] =
    P( spaces.? ~ stmt.repX(0, spaces) ~ spaces.? ).map(_.flatten)
  def eval_input[_: P]: P[AST.expr] = P( testlist ~ NEWLINE.rep ~ ENDMARKER ).map(tuplize)

  def collapse_dotted_name(name: Seq[AST.identifier]): AST.expr = {
    name.tail.foldLeft[AST.expr](AST.expr.Name(name.head, AST.expr_context.Load))(
      (x, y) => AST.expr.Attribute(x, y, AST.expr_context.Load)
    )
  }

  def decorator[_: P]: P[AST.expr] = P( "@" ~/ dotted_name ~ ("(" ~ arglist ~ ")" ).?  ~~ LexicalParser.nonewlinewscomment.? ~~ NEWLINE).map{
    case (name, None) => collapse_dotted_name(name)
    case (name, Some((args, (keywords, starargs, keywordargs)))) =>
      val x = collapse_dotted_name(name)
      AST.expr.Call(x, args, keywords, starargs, keywordargs)
  }

  def decorators[_: P] = P( decorator.rep )
  def decorated[_: P]: P[AST.stmt] = P( decorators ~ (classdef | funcdef) ).map{case (a, b) => b(a)}
  def classdef[_: P]: P[Seq[AST.expr] => AST.stmt.ClassDef] =
    P( keyword("class") ~/ NAME ~ ("(" ~ testlist.? ~ ")").?.map(_.toSeq.flatten.flatten) ~ ":" ~~ suite ).map{
      case (a, b, c) => AST.stmt.ClassDef(a, b, c, _)
    }


  def funcdef[_: P]: P[Seq[AST.expr] => AST.stmt.FunctionDef] = P( keyword("def") ~/ NAME ~ parameters ~ ":" ~~ suite ).map{
    case (name, args, suite) => AST.stmt.FunctionDef(name, args, suite, _)
  }
  def parameters[_: P]: P[AST.arguments] = P( "(" ~ varargslist ~ ")" )

  def stmt[_: P]: P[Seq[AST.stmt]] = P( compound_stmt.map(Seq(_)) | simple_stmt )

  def simple_stmt[_: P]: P[Seq[AST.stmt]] = P( small_stmt.rep(1, sep = ";") ~ ";".? )
  def small_stmt[_: P]: P[AST.stmt] = P(
    print_stmt  | del_stmt | pass_stmt | flow_stmt |
      import_stmt | global_stmt | exec_stmt | assert_stmt | expr_stmt
  )
  def expr_stmt[_: P]: P[AST.stmt] = {
    def aug = P( testlist ~ augassign ~ (yield_expr | testlist.map(tuplize)) )
    def assign = P( testlist ~ ("=" ~ (yield_expr | testlist.map(tuplize))).rep )

    P(
      aug.map{case (a, b, c) => AST.stmt.AugAssign(tuplize(a), b, c) } |
        assign.map{
          case (a, Nil) => AST.stmt.Expr(tuplize(a))
          case (a, b) => AST.stmt.Assign(Seq(tuplize(a)) ++ b.init, b.last)
        }
    )
  }

  def augassign[_: P]: P[AST.operator] = P(
    "+=".!.map(_ => AST.operator.Add) |
      "-=".!.map(_ => AST.operator.Sub) |
      "*=".!.map(_ => AST.operator.Mult) |
      "/=".!.map(_ => AST.operator.Div) |
      "%=".!.map(_ => AST.operator.Mod) |
      "&=".!.map(_ => AST.operator.BitAnd) |
      "|=".!.map(_ => AST.operator.BitOr) |
      "^=".!.map(_ => AST.operator.BitXor) |
      "<<=".!.map(_ => AST.operator.LShift) |
      ">>=".!.map(_ => AST.operator.RShift) |
      "**=".!.map(_ => AST.operator.Pow) |
      "//=".!.map(_ => AST.operator.FloorDiv)
  )

  def print_stmt[_: P]: P[AST.stmt.Print] = {
    def noDest = P( test.rep(sep = ",") ~ ",".?).map(AST.stmt.Print(None, _, true))
    def dest = P( ">>" ~ test ~ ("," ~ test).rep ~ ",".?).map{case (dest, exprs) => AST.stmt.Print(Some(dest), exprs, true)}
    P( "print" ~~ " ".rep ~~ (noDest | dest) )
  }
  def del_stmt[_: P] = P( keyword("del") ~~ " ".rep ~~ exprlist ).map(AST.stmt.Delete)
  def pass_stmt[_: P] = P( keyword("pass") ).map(_ => AST.stmt.Pass)
  def flow_stmt[_: P]: P[AST.stmt] = P( break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt )
  def break_stmt[_: P] = P( keyword("break") ).map(_ => AST.stmt.Break)
  def continue_stmt[_: P] = P( keyword("continue") ).map(_ => AST.stmt.Continue)
  def return_stmt[_: P] = P( keyword("return") ~~ " ".rep ~~ testlist.map(tuplize).? ).map(AST.stmt.Return)

  def yield_stmt[_: P] = P( yield_expr ).map(AST.stmt.Expr)
  def raise_stmt[_: P]: P[AST.stmt.Raise] = P( keyword("raise") ~~ " ".rep ~~test.? ~ ("," ~ test).? ~ ("," ~ test).? ).map(AST.stmt.Raise.tupled)
  def import_stmt[_: P]: P[AST.stmt] = P( import_name | import_from )
  def import_name[_: P]: P[AST.stmt.Import] = P( keyword("import") ~ dotted_as_names ).map(AST.stmt.Import)
  def import_from[_: P]: P[AST.stmt.ImportFrom] = {
    def named = P( ".".rep(1).!.? ~ dotted_name.!.map(Some(_)) )
    def unNamed = P( ".".rep(1).!.map(x => (Some(x), None)) )
    def star = P( "*".!.map(_ => Seq(AST.alias(AST.identifier("*"), None))) )
    P( keyword("from") ~ (named | unNamed) ~ keyword("import") ~ (star | "(" ~ import_as_names ~ ")" | import_as_names) ).map{
      case (dots, module, names) => AST.stmt.ImportFrom(module.map(AST.identifier), names, dots.map(_.length))
    }
  }
  def import_as_name[_: P]: P[AST.alias] = P( NAME ~ (keyword("as") ~ NAME).? ).map(AST.alias.tupled)
  def dotted_as_name[_: P]: P[AST.alias] = P( dotted_name.map(x => AST.identifier(x.map(_.name).mkString("."))) ~ (keyword("as") ~ NAME).? ).map(AST.alias.tupled)
  def import_as_names[_: P] = P( import_as_name.rep(1, ",") ~ (",").? )
  def dotted_as_names[_: P] = P( dotted_as_name.rep(1, ",") )
  def dotted_name[_: P] = P( NAME.rep(1, ".") )
  def global_stmt[_: P]: P[AST.stmt.Global] = P( keyword("global") ~ NAME.rep(sep = ",") ).map(AST.stmt.Global)
  def exec_stmt[_: P]: P[AST.stmt.Exec] = P( keyword("exec") ~ expression ~ (keyword("in") ~ test ~ ("," ~ test).?).? ).map {
    case (expr, None) => AST.stmt.Exec(expr, None, None)
    case (expr, Some((globals, None))) => AST.stmt.Exec(expr, Some(globals), None)
    case (expr, Some((globals, Some(locals)))) => AST.stmt.Exec(expr, Some(globals), Some(locals))
  }
  def assert_stmt[_: P]: P[AST.stmt.Assert] = P( keyword("assert") ~ test ~ ("," ~ test).? ).map(AST.stmt.Assert.tupled)

  def compound_stmt[_: P]: P[AST.stmt] = P( if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | decorated )
  def if_stmt[_: P]: P[AST.stmt.If] = {
    def firstIf = P( keyword("if") ~/ test ~ ":" ~~ suite )
    def elifs = P( (space_indents ~~ keyword("elif") ~/ test ~ ":" ~~ suite).repX )
    def lastElse = P( (space_indents ~~ keyword("else") ~/ ":" ~~ suite).? )
    P( firstIf ~~ elifs ~~ lastElse ).map{
      case (test, body, elifs, orelse) =>
        val (init :+ last) = (test, body) +: elifs
        val (last_test, last_body) = last
        init.foldRight(AST.stmt.If(last_test, last_body, orelse.toSeq.flatten)){
          case ((test, body), rhs) => AST.stmt.If(test, body, Seq(rhs))
        }
    }
  }
  def space_indents[_: P] = P( spaces.repX ~~ " ".repX(indent) )
  def while_stmt[_: P] = P( keyword("while") ~/ test ~ ":" ~~ suite ~~ (space_indents ~~ keyword("else") ~/ ":" ~~ suite).?.map(_.toSeq.flatten) ).map(AST.stmt.While.tupled)
  def for_stmt[_: P]: P[AST.stmt.For] = P( keyword("for") ~/ exprlist ~ keyword("in") ~ testlist ~ ":" ~~ suite ~~ (space_indents ~ keyword("else") ~/ ":" ~~ suite).? ).map {
    case (itervars, generator, body, orelse) =>
      AST.stmt.For(tuplize(itervars), tuplize(generator), body, orelse.toSeq.flatten)
  }
  def try_stmt[_: P]: P[AST.stmt]= {
    def `try` = P( keyword("try") ~/ ":" ~~ suite )
    def excepts: P[Seq[AST.excepthandler]] = P( (except_clause ~ ":" ~~ suite).map{
      case (None, body) => AST.excepthandler.ExceptHandler(None, None, body)
      case (Some((x, None)), body) => AST.excepthandler.ExceptHandler(Some(x), None, body)
      case (Some((x, Some(y))), body) => AST.excepthandler.ExceptHandler(Some(x), Some(y), body)
    }.repX )
    def `else` = P( space_indents ~~ keyword("else") ~/ ":" ~~ suite )
    def `finally` = P( space_indents ~~ keyword("finally") ~/ ":" ~~ suite )
    P( `try` ~~ excepts ~~ `else`.? ~~ `finally`.? ).map{
      case (tryBlock, excepts, elseBlock, None) =>
        AST.stmt.TryExcept(tryBlock, excepts, elseBlock.toSeq.flatten)
      case (tryBlock, Nil, None, Some(finallyBlock)) =>
        AST.stmt.TryFinally(tryBlock, finallyBlock)
      case (tryBlock, excepts, elseBlock, Some(finallyBlock)) =>
        AST.stmt.TryFinally(
          Seq(AST.stmt.TryExcept(tryBlock, excepts, elseBlock.toSeq.flatten)),
          finallyBlock
        )
    }
  }
  def with_stmt[_: P]: P[AST.stmt.With] = P( keyword("with") ~/ with_item.rep(1, ",")~ ":" ~~ suite ).map{
    case (items, body) =>
      val (last_expr, last_vars) = items.last
      val inner = AST.stmt.With(last_expr, last_vars, body)
      items.init.foldRight(inner){
        case ((expr, vars), body) => AST.stmt.With(expr, vars, Seq(body))
      }
  }
  def with_item[_: P]: P[(AST.expr, Option[AST.expr])] = P( test ~ (keyword("as") ~ expression).? )
  // NB compile.c makes sure that the default except clause is last
  def except_clause[_: P] = P( space_indents ~ keyword("except") ~/ (test ~ ((keyword("as") | ",") ~ test).?).? )


  def suite[_: P]: P[Seq[AST.stmt]] = {
    def deeper: P[Int] = {
      def commentLine = P("\n" ~~ LexicalParser.nonewlinewscomment.?.map(_ => 0)).map((_, Some("")))
      def endLine = P("\n" ~~ (" "|"\t").repX(indent + 1).!.map(_.length) ~~ LexicalParser.comment.!.? )
      P( LexicalParser.nonewlinewscomment.? ~~ ( endLine | commentLine ).repX(1) ).map{
        _.collectFirst{ case (s, None) => s}
      }.filter(_.isDefined).map(_.get)
    }
    def indented = P( deeper.flatMapX{ nextIndent =>
      new Statements(nextIndent).stmt.repX(1, spaces.repX(1) ~~ (" " * nextIndent | "\t" * nextIndent)).map(_.flatten)
    } )
    P( indented | " ".rep ~ simple_stmt )
  }
}
