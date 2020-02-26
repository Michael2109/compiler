package compiler.parser

import fastparse.NoWhitespace._
import fastparse._
import compiler.ast.AST
import compiler.ast.AST._

import scala.collection.mutable
import scala.language.implicitConversions
import fastparse._


object ExpressionParser {

  implicit def whitespace(cfg: P[_]): P[Unit] = LexicalParser.wscomment(cfg)
  def accessModifier[_: P]: P[Modifier] = P(LexicalParser.kw("protected")).map(_ => Protected()) | P(LexicalParser.kw("private")).map(_ => Private()) | P(LexicalParser.kw("local")).map(_ => PackageLocal())

  def annotationParser[_: P]: P[Annotation] = P("@" ~ nameParser).map(Annotation)

  def parensParser[_: P]: P[Expression] = P( "(" ~ (expressionParser) ~ ")" )
  def termParser[_: P]: P[Expression] = P(Chain(allExpressionsParser, multiply | divide ))
  def arith_exprParser[_: P]: P[Expression] = P(Chain(termParser, add | subtract))
  def rExprParser[_: P]: P[Expression] = P(Chain(arith_exprParser, LtE | Lt | GtE | Gt))

  def allExpressionsParser[_: P] = methodCallParser | newClassInstanceParser | ternaryParser | numberParser | identifierParser | stringLiteral | parensParser

  def expressionParser[_: P]: P[Expression] = {

    P(Chain(rExprParser, and | or)).rep(sep = ".").map(expressions => {
      expressions.length match {
        case 0 => BlockExpr(Seq())
        case 1 => expressions.head
        case _ => NestedExpr(expressions)
      }
    })
  }

  def identifierParser[_: P]: P[AST.Identifier] = LexicalParser.identifier.map(x => Identifier(Name(x)))

  def finalModifierParser[_: P]: P[AST.Final.type] = P("final").map(x => Final)

  def methodCallParser[_: P]: P[MethodCall] = P(nameParser ~ "(" ~ expressionParser.rep(sep = ",") ~ ")").map(x => MethodCall(x._1, x._2))

  def modifiers[_: P]: P[Seq[Modifier]] = P(accessModifier | typeModifier).rep

  def nameParser[_: P]: P[Name] = LexicalParser.identifier.map(x => Name(x))

  def newClassInstanceParser[_: P]: P[NewClassInstance] = P(LexicalParser.kw("new") ~ typeRefParser ~ LexicalParser.kw("(") ~ expressionParser.rep(sep = ",") ~ LexicalParser.kw(")")).map(x => NewClassInstance(x._1, x._2, None))

  def numberParser[_: P]: P[Expression] = P(LexicalParser.floatnumber ~ P("F" | "f")).map(FloatConst) | P(LexicalParser.longinteger).map(LongConst) | P(LexicalParser.floatnumber).map(DoubleConst) | P(LexicalParser.integer).map(IntConst)

  def stringLiteral[_: P]: P[StringLiteral] = LexicalParser.stringliteral.map(x => StringLiteral(x))

  def ternaryParser[_: P]: P[Ternary] = P(LexicalParser.kw("if") ~ expressionParser ~ "then" ~ expressionParser ~ "else" ~ expressionParser).map(x => Ternary(x._1, x._2, x._3))

  def typeModifier[_: P]: P[Modifier] = P(LexicalParser.kw("mutable")).map(_ => Final()) | P(LexicalParser.kw("abstract")).map(_ => Abstract()) | P(LexicalParser.kw("pure")).map(_ => Pure())

  def typeRefParser[_: P]: P[Type] = refParser.map(Type)

  def refParser[_: P]: P[Ref] = P(nameParser.rep(sep = ".", min=2)).map(x => RefQual(QualName(NameSpace(x.dropRight(1)), x.last))) | P(nameParser).map(RefLocal)

  private def Chain[_: P](p: P[Expression], op: P[AST.Operator]) = P(p ~ (op ~ p).rep).map {
    case (lhs, chunks) =>
      chunks.foldLeft(lhs) { case (lhs, (operator, rhs)) =>
        operator match {
          case op: ABinOp => new ABinary(op, lhs, rhs)
          case op: BBinOp => new BBinary(op, lhs, rhs)
          case op: RBinOp => new RBinary(op, lhs, rhs)
        }
      }
  }

  def op[T, _: P](s: => P[Unit], rhs: T) = s.!.map(_ => rhs)
  def Lt[_: P] = op("<", AST.Less)
  def Gt[_: P] = op(">", AST.Greater.asInstanceOf[Operator])
  def Eq[_: P] = op("==", AST.Equal.asInstanceOf[Operator])
  def GtE[_: P] = op(">=", AST.GreaterEqual.asInstanceOf[Operator])
  def LtE[_: P] = op("<=", AST.LessEqual.asInstanceOf[Operator])
  def comp_op[_: P] = P(LtE | GtE | Eq | Gt | Lt)
  def add[_: P] = op("+", AST.Add.asInstanceOf[Operator])
  def subtract[_: P] = op("-", AST.Subtract.asInstanceOf[Operator])
  def multiply [_: P]= op("*", AST.Multiply.asInstanceOf[Operator])
  def divide[_: P] = op("/", AST.Divide.asInstanceOf[Operator])
  def and[_: P] = op("&&", AST.And.asInstanceOf[Operator])
  def or[_: P] = op("||", AST.Or.asInstanceOf[Operator])
}
