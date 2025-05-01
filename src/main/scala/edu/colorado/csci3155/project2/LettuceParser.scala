package edu.colorado.csci3155.project2

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical

class LettuceParser extends RegexParsers {

    def floatingPointNumber: Parser[String] = {
        """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    }

    def identifier: Parser[String] = {
        """[a-zA-Z_][a-zA-Z0-9_]*""".r
    }
    def LetKwd: Parser[String] = {
        "let"  | "Let"
    }

    def compOperator: Parser[String] = {
        ">=" | "<=" | ">" | "<" | "==" | "!="
    }


    def funDefinition: Parser[FunDef] = {
        ( ( "function" |"fun"|"fun1") ~ "(" ) ~> (identifier) ~ (")" ~> exprLev1)  ^^ {
            case id~e => FunDef(id, e)
        }
    }

    def funDefinition2: Parser[FunDef] = {
        (("fun2"~"(") ~> (identifier)) ~ ((")" ~ "(") ~> (identifier)) ~ ((")" ~> exprLev1))^^{
            case id1~id2~e => FunDef(id1, FunDef(id2, e))

        }
    }


    def funDefinition3: Parser[FunDef] = {
        (("fun3"~"(") ~> (identifier)) ~ ((")" ~ "(") ~> (identifier)) ~ ((")" ~ "(") ~> (identifier)) ~ ((")" ~> exprLev1))^^{
            case id1~id2~id3~e => FunDef(id1, FunDef(id2, FunDef(id3, e)))
        }
    }


    def funCallArgs: Parser[Expr] = {
        "(" ~> exprLev1 <~ ")"
    }

    def exprLev1: Parser[Expr] = {
        val opt1 = ("let" ~> identifier) ~ ("=" ~> exprLev1) ~ ("in" ~> exprLev1)  ^^ {
            case s1 ~ e1 ~ e2 => Let(s1, e1, e2)
        }
        val opt2 = ("letrec" ~> identifier) ~ ("=" ~> funDefinition) ~ ("in" ~> exprLev1 ) ^^ {
            case s1 ~ (FunDef(id, e1)) ~ e2 => {
                LetRec(s1, id, e1,  e2)
            }
        }

        val opt3 = (funDefinition | funDefinition2 | funDefinition3) ^^ { s => s }
        

        val opt4 = ("if" ~> exprLev2)~("then" ~> exprLev1)~("else" ~> exprLev1) ^^ {
            case e ~ e1 ~ e2 => IfThenElse(e, e1, e2)
        }

        val opt5 = exprLev2~ opt(("&&"|"||") ~ exprLev1)^^ {
            case e1 ~ Some("&&" ~ e2) => And(e1, e2)
            case e1 ~ Some("||" ~ e2) => Or(e1, e2)
            case e1 ~ None => e1
        }
      

        opt1 | opt2 | opt3 | opt4 |  opt5 
    }

    def exprLev2: Parser[Expr] = {
        exprLev3 ~ opt(compOperator~ exprLev2) ^^{
            case e1~Some(">="~e2) => Geq(e1, e2)
            case e1~Some("<="~e2) => Geq(e2, e1)
            case e1~Some("=="~e2) => Eq(e1, e2)
            case e1~Some("!="~e2) => Neq(e1, e2)
            case e1~Some(">"~e2) => Gt(e1, e2)
            case e1~Some("<"~e2) => Gt(e2, e1)
            case e1~None => e1
        }
    }

    def exprLev3: Parser[Expr] = {
        exprLev4 ~ opt( ("->" | "+|+"| "+^+" | "+"| "-"  ) ~ exprLev3 ) ^^ {
            case e1 ~ Some("->" ~ e2) => Translate(e1, e2)
            case e1 ~ Some("+^+" ~ e2) => VConcat(e1, e2)
            case e1 ~ Some("+|+" ~ e2) => HConcat (e1, e2)
            case e1 ~ Some("+" ~ e2) => Plus(e1, e2)
            case e1 ~ Some("-" ~ e2) => Minus(e1, e2)
            case e1 ~ None => e1
        }
    }

    def exprLev4: Parser[Expr] = {

        exprLev5 ~ opt(("**"| "//"| "*"|"/"|"~" ) ~ exprLev4) ^^ {
            case e1 ~ Some("**" ~ e2) => Scale(e1, e2)
            case e1 ~ Some("//" ~ e2) => Rotate(e1, e2)
            case e1 ~ Some("~" ~ e2) => Overlay(e1, e2)
            case e1 ~ Some("*" ~ e2) => Mult(e1, e2)
            case e1 ~ Some("/" ~ e2) => Div(e1, e2)
            case e1 ~ None => e1
        }

    }

    def exprLev5: Parser[Expr] = {
        ( floatingPointNumber ^^ { s => Const(s.toFloat)} ) |
          (  "true"^^{ _ => ConstBool(true) } ) |
          ( "false" ^^{ _ => ConstBool(false) } ) |
          (("[" ~> (exprLev1 ~ "," ~ exprLev1 ) <~"]") ^^{
            case e1 ~","~e2 => Pair(e1, e2)
          }) | 
          (  "(" ~> exprLev1 <~ ")" ) |
          ( ( "rectangle" | "triangle" | "circle" | "hline" | "vline"| "first"| "second"| "ref_x" | "ref_y" | "cos" | "sin" | "!"  ) ~ ("(" ~> exprLev1 <~ ")") ^^{
              case "rectangle"~e => Rectangle(e)
              case "triangle"~e => EquiTriangle(e)
              case "circle"~e => Circle(e)
              case "hline"~e => HLine(e)
              case "vline"~e => VLine(e)
              case "ref_x" ~ e => ReflectX(e)
              case "ref_y" ~ e => ReflectY(e)
              case "first"~e => PairFirst(e)
              case "second"~e => PairSecond(e)
              case "cos"~ e => Cosine(e)
              case "sin"~ e => Sine(e)
              case "!"~e => Not(e)
          } ) |
          ( identifier ~ rep(funCallArgs)  ^^ {
              case s~Nil => Ident(s)
              case s~l => l.foldLeft[Expr] (Ident(s)) { case (e, lj) => FunCall(e, lj) }
          })
    }

    def parseString(s: String): Program = {
        val e= parseAll(exprLev1, s)
        e match {
            case Success(p, _) => TopLevel(p)
            case Failure(msg, _) => throw new IllegalArgumentException("Failure:" + msg)
            case Error(msg, _) => throw new IllegalArgumentException("Error: " + msg)
        }
    }
}
