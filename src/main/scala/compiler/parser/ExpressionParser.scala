package compiler.parser

import fastparse._
import compiler.ast.AST
import compiler.ast.AST._

import scala.collection.mutable
import scala.language.implicitConversions
import fastparse._


object ExpressionParser {

  implicit def whitespace(cfg: P[_]): P[Unit] = LexicalParser.wscomment(cfg)

  def accessModifier[_: P]: P[Modifier] = P(LexicalParser.kw("protected")).map(_ => Protected) | P(LexicalParser.kw("private")).map(_ => Private) | P(LexicalParser.kw("local")).map(_ => PackageLocal)

  def annotationParser[_: P]: P[Annotation] = P("@" ~ nameParser).map(Annotation)

  def parenthesisParser[_: P]: P[Expression] = P( "(" ~ (expressionParser) ~ ")" )
  def termParser[_: P]: P[Expression] = P(Chain(simpleExpressionParser, multiply | divide ))
  def arithmeticExpressionParser[_: P]: P[Expression] = P(Chain(termParser, add | subtract))
  def relationalExpressionParser[_: P]: P[Expression] = P(Chain(arithmeticExpressionParser, LtE | Lt | GtE | Gt))

  def simpleExpressionParser[_: P]: P[Expression] = methodCallParser | newClassInstanceParser | ternaryParser | numberParser | identifierParser | stringLiteral | parenthesisParser

  def expressionParser[_: P]: P[Expression] = {

    P(Chain(relationalExpressionParser, and | or)).rep(sep = ".", min = 1 ).map(expressions => {
      expressions.length match {
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

  def typeModifier[_: P]: P[Modifier] = P(LexicalParser.kw("mutable")).map(_ => Final) | P(LexicalParser.kw("abstract")).map(_ => Abstract) | P(LexicalParser.kw("pure")).map(_ => Pure)

  def typeRefParser[_: P]: P[Type] = refParser.map(Type)

  def refParser[_: P]: P[Ref] = P(nameParser.rep(sep = ".", min=2)).map(x => RefQual(QualName(NameSpace(x.dropRight(1)), x.last))) | P(nameParser).map(RefLocal)

  def Chain[_: P](p: => P[Expression], op: => P[Operator]): P[Expression] = P( p ~ (op ~ p).rep ).map{
    case (lhs, chunks) =>
      chunks.foldLeft(lhs){case (lhs, (op, rhs)) =>
        op match {
          case op: ABinOp => new ABinary(op, lhs, rhs)
          case op: BBinOp => new BBinary(op, lhs, rhs)
          case op: RBinOp => new RBinary(op, lhs, rhs)
        }
      }
  }

  def op[T, _: P](s: => P[Unit], rhs: T) = s.!.map(_ => rhs)
  def Lt[_: P]: P[Operator] = op("<", AST.Less)
  def Gt[_: P]: P[Operator] = op(">", AST.Greater)
  def Eq[_: P]: P[Operator] = op("==", AST.Equal)
  def GtE[_: P]: P[Operator] = op(">=", AST.GreaterEqual)
  def LtE[_: P]: P[Operator] = op("<=", AST.LessEqual)
  def comp_op[_: P]: P[Operator] = P(LtE | GtE | Eq | Gt | Lt)
  def add[_: P]: P[Operator] = op("+", AST.Add)
  def subtract[_: P]: P[Operator] = op("-", AST.Subtract)
  def multiply [_: P]: P[Operator] = op("*", AST.Multiply)
  def divide[_: P]: P[Operator] = op("/", AST.Divide)
  def and[_: P]: P[Operator] = op("&&", AST.And)
  def or[_: P]: P[Operator] = op("||", AST.Or)
}
