package transpiler.parser

import fastparse.NoWhitespace._
import fastparse._
import transpiler.ast.AST
import transpiler.ast.AST._

import scala.collection.mutable
import scala.language.implicitConversions
import fastparse._

import LexicalParser.keyword

object ExpressionParser {

  implicit def whitespace(cfg: P[_]): P[Unit] = LexicalParser.wscomment(cfg)
  def tuplize(xs: Seq[AST.expr]) = xs match{
    case Seq(x) => x
    case xs => AST.expr.Tuple(xs, AST.expr_context.Load)
  }

  def NAME[_: P]: P[AST.identifier] = LexicalParser.identifier
  def NUMBER[_: P]: P[AST.expr.Num] = P( LexicalParser.floatnumber | LexicalParser.longinteger | LexicalParser.integer | LexicalParser.imagnumber ).map(AST.expr.Num)
  def STRING[_: P]: P[AST.string] = LexicalParser.stringliteral

  def test[_: P]: P[AST.expr] = {
    def ternary = P( or_test ~ (keyword("if") ~ or_test ~ keyword("else") ~ test).? ).map{
      case (x, None) => x
      case (x, Some((test, neg))) => AST.expr.IfExp(test, x, neg)
    }
    P( ternary | lambdef )
  }
  def or_test[_: P] = P( and_test.rep(1, sep = keyword("or")) ).map{
    case Seq(x) => x
    case xs => AST.expr.BoolOp(AST.boolop.Or, xs)
  }
  def and_test[_: P] = P( not_test.rep(1, sep = keyword("and")) ).map{
    case Seq(x) => x
    case xs => AST.expr.BoolOp(AST.boolop.And, xs)
  }
  def not_test[_: P]: P[AST.expr] = P( (keyword("not") ~ not_test).map(AST.expr.UnaryOp(AST.unaryop.Not, _)) | comparison )
  def comparison[_: P]: P[AST.expr] = P( expression ~ (comp_op ~ expression).rep ).map{
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      AST.expr.Compare(lhs, ops, vals)
  }

  // Common operators, mapped from their
  // strings to their type-safe representations
  def op[T, _: P](s: => P[Unit], rhs: T) = s.!.map(_ => rhs)
  def LShift[_: P] = op("<<", AST.operator.LShift)
  def RShift[_: P] = op(">>", AST.operator.RShift)
  def Lt[_: P] = op("<", AST.cmpop.Lt)
  def Gt[_: P] = op(">", AST.cmpop.Gt)
  def Eq[_: P] = op("==", AST.cmpop.Eq)
  def GtE[_: P] = op(">=", AST.cmpop.GtE)
  def LtE[_: P] = op("<=", AST.cmpop.LtE)
  def NotEq[_: P] = op("<>" | "!=", AST.cmpop.NotEq)
  def In[_: P] = op(keyword("in"), AST.cmpop.In)
  def NotIn[_: P] = op(keyword("not") ~ keyword("in"), AST.cmpop.NotIn)
  def Is[_: P] = op(keyword("is"), AST.cmpop.Is)
  def IsNot[_: P] = op(keyword("is") ~ keyword("not"), AST.cmpop.IsNot)
  def comp_op[_: P] = P( LtE|GtE|Eq|Gt|Lt|NotEq|In|NotIn|IsNot|Is )
  def Add[_: P] = op("+", AST.operator.Add)
  def Sub[_: P] = op("-", AST.operator.Sub)
  def Pow[_: P] = op("**", AST.operator.Pow)
  def Mult[_: P] = op("*", AST.operator.Mult)
  def Div[_: P] = op("/", AST.operator.Div)
  def Mod[_: P] = op("%", AST.operator.Mod)
  def FloorDiv[_: P] = op("//", AST.operator.FloorDiv)
  def BitOr[_: P] = op("|", AST.operator.BitOr)
  def BitAnd[_: P] = op("&", AST.operator.BitAnd)
  def BitXor[_: P] = op("^", AST.operator.BitXor)
  def UAdd[_: P] = op("+", AST.unaryop.UAdd)
  def USub[_: P] = op("-", AST.unaryop.USub)
  def Invert[_: P] = op("~", AST.unaryop.Invert)
  def unary_op[_: P] = P ( UAdd | USub | Invert )


  def Unary[_: P](p: => P[AST.expr]) =
    (unary_op ~ p).map{ case (op, operand) => AST.expr.UnaryOp(op, operand) }

  def Chain[_: P](p: => P[AST.expr], op: => P[AST.operator]) = P( p ~ (op ~ p).rep ).map{
    case (lhs, chunks) =>
      chunks.foldLeft(lhs){case (lhs, (op, rhs)) =>
        AST.expr.BinOp(lhs, op, rhs)
      }
  }
  def expression[_: P]: P[AST.expr] = P( Chain(xor_expr, BitOr) )
  def xor_expr[_: P]: P[AST.expr] = P( Chain(and_expr, BitXor) )
  def and_expr[_: P]: P[AST.expr] = P( Chain(shift_expr, BitAnd) )
  def shift_expr[_: P]: P[AST.expr] = P( Chain(arith_expr, LShift | RShift) )

  def arith_expr[_: P]: P[AST.expr] = P( Chain(term, Add | Sub) )
  def term[_: P]: P[AST.expr] = P( Chain(factor, Mult | FloorDiv | Div | Mod ) )

  def factor[_: P]: P[AST.expr] = P( power | Unary(factor) )
  def power[_: P]: P[AST.expr] = P( atom ~ trailer.rep ~ (Pow ~ factor).? ).map{
    case (lhs, trailers, rhs) =>
      val left = trailers.foldLeft(lhs)((l, t) => t(l))
      rhs match{
        case None => left
        case Some((op, right)) => AST.expr.BinOp(left, op, right)
      }
  }
  def atom[_: P]: P[AST.expr] = {
    def empty_tuple = ("(" ~ ")").map(_ => AST.expr.Tuple(Nil, AST.expr_context.Load))
    def empty_list = ("[" ~ "]").map(_ => AST.expr.List(Nil, AST.expr_context.Load))
    def empty_dict = ("{" ~ "}").map(_ => AST.expr.Dict(Nil, Nil))
    P(
      empty_tuple  |
        empty_list |
        empty_dict |
        "(" ~ (yield_expr | generator | tuple | test) ~ ")" |
        "[" ~ (list_comp | list) ~ "]" |
        "{" ~ dictorsetmaker ~ "}" |
        "`" ~ testlist1.map(x => AST.expr.Repr(AST.expr.Tuple(x, AST.expr_context.Load))) ~ "`" |
        STRING.rep(1).map(_.mkString).map(AST.expr.Str) |
        NAME.map(AST.expr.Name(_, AST.expr_context.Load)) |
        NUMBER
    )
  }
  def list_contents[_: P] = P( test.rep(1, ",") ~ ",".? )
  def list[_: P] = P( list_contents ).map(AST.expr.List(_, AST.expr_context.Load))
  def tuple_contents[_: P] = P( test ~ "," ~ list_contents.?).map { case (head, rest)  => head +: rest.getOrElse(Seq.empty) }
  def tuple[_: P] = P( tuple_contents).map(AST.expr.Tuple(_, AST.expr_context.Load))
  def list_comp_contents[_: P] = P( test ~ comp_for.rep(1) )
  def list_comp[_: P] = P( list_comp_contents ).map(AST.expr.ListComp.tupled)
  def generator[_: P] = P( list_comp_contents ).map(AST.expr.GeneratorExp.tupled)

  def lambdef[_: P]: P[AST.expr.Lambda] = P( keyword("lambda") ~ varargslist ~ ":" ~ test ).map(AST.expr.Lambda.tupled)
  def trailer[_: P]: P[AST.expr => AST.expr] = {
    def call = P("(" ~ arglist ~ ")").map{ case (args, (keywords, starargs, keywordargs)) => (lhs: AST.expr) => AST.expr.Call(lhs, args, keywords, starargs, keywordargs)}
    def slice = P("[" ~ subscriptlist ~ "]").map(args => (lhs: AST.expr) => AST.expr.Subscript(lhs, args, AST.expr_context.Load))
    def attr = P("." ~ NAME).map(id => (lhs: AST.expr) => AST.expr.Attribute(lhs, id, AST.expr_context.Load))
    P( call | slice | attr )
  }
  def subscriptlist[_: P] = P( subscript.rep(1, ",") ~ ",".? ).map{
    case Seq(x) => x
    case xs => AST.slice.ExtSlice(xs)
  }
  def subscript[_: P]: P[AST.slice] = {
    def ellipses = P( ("." ~ "." ~ ".").map(_ => AST.slice.Ellipsis) )
    def single = P( test.map(AST.slice.Index) )
    def multi = P(test.? ~ ":" ~ test.? ~ sliceop.?).map { case (lower, upper, step) =>
      AST.slice.Slice(
        lower,
        upper,
        step.map(_.getOrElse(AST.expr.Name(AST.identifier("None"), AST.expr_context.Load)))
      )
    }
    P( ellipses | multi | single )
  }

  def sliceop[_: P] = P( ":" ~ test.? )
  def exprlist[_: P]: P[Seq[AST.expr]] = P( expression.rep(1, sep = ",") ~ ",".? )
  def testlist[_: P]: P[Seq[AST.expr]] = P( test.rep(1, sep = ",") ~ ",".? )
  def dictorsetmaker[_: P]: P[AST.expr] = {
    def dict_item = P( test ~ ":" ~ test )
    def dict: P[AST.expr.Dict] = P(
      (dict_item.rep(1, ",") ~ ",".?).map{x =>
        val (keys, values) = x.unzip
        AST.expr.Dict(keys, values)
      }
    )
    def dict_comp = P(
      (dict_item ~ comp_for.rep(1)).map(AST.expr.DictComp.tupled)
    )
    def set: P[AST.expr.Set] = P( test.rep(1, ",") ~ ",".? ).map(AST.expr.Set)
    def set_comp = P( test ~ comp_for.rep(1) ).map(AST.expr.SetComp.tupled)
    P( dict_comp | dict | set_comp | set)
  }

  def arglist[_: P] = {
    def inits = P( (plain_argument ~ !"=").rep(0, ",") )
    def later = P( named_argument.rep(0, ",") ~ ",".? ~ ("*" ~ test).? ~ ",".? ~ ("**" ~ test).? ~ ",".? ~ named_argument.rep(0, ",")).map{
      case (named1, dot, star, named2) => (named1 ++ named2, dot, star )
    }
    P( inits ~ ",".? ~ later )
  }

  def plain_argument[_: P] = P( test ~ comp_for.rep ).map{
    case (x, Nil) => x
    case (x, gens) => AST.expr.GeneratorExp(x, gens)
  }
  def named_argument[_: P] = P( NAME ~ "=" ~ test  ).map(AST.keyword.tupled)

  def comp_for[_: P]: P[AST.comprehension] = P( keyword("for") ~ exprlist ~ keyword("in") ~ or_test ~ comp_if.rep ).map{
    case (targets, test, ifs) => AST.comprehension(tuplize(targets), test, ifs)
  }
  def comp_if[_: P]: P[AST.expr] = P( keyword("if") ~ test )

  def testlist1[_: P]: P[Seq[AST.expr]] = P( test.rep(1, sep = ",") )

  // not used in grammar, but may appear in "node" passed from Parser to Compiler
  //  def encoding_decl[_: P]: P0 = P( NAME )

  def yield_expr[_: P]: P[AST.expr.Yield] = P( keyword("yield") ~ testlist.map(tuplize).? ).map(AST.expr.Yield)

  def varargslist[_: P]: P[AST.arguments] = {
    def named_arg = P( fpdef ~ ("=" ~ test).? )
    def x = P( named_arg.rep(sep = ",") ~ ",".? ~ ("*" ~ NAME).? ~ ",".? ~ ("**" ~ NAME).? ).map{
      case (normal_args, starargs, keywordargs) =>
        val (args, defaults) = normal_args.unzip
        AST.arguments(args, starargs, keywordargs, defaults.flatten)
    }
    P( x )
  }

  def fpdef[_: P]: P[AST.expr] = P( NAME.map(AST.expr.Name(_, AST.expr_context.Param)) | "(" ~ fplist ~ ")" )
  def fplist[_: P]: P[AST.expr] = P( fpdef.rep(sep = ",") ~ ",".? ).map(AST.expr.Tuple(_, AST.expr_context.Param))
}
