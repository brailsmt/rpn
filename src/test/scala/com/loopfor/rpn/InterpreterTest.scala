package com.loopfor.rpn

import scala.io.{Source, StdIn}
import scala.util.{Failure, Success}

object InterpreterTest {
  def main(args: Array[String]): Unit = {
    val lexer = BasicLexer()
    val parser = BasicParser()
    val generator = BasicGenerator()
    val optimizer = BasicOptimizer()
    val loader = BasicLoader()
    val evaluator = BasicEvaluator { name =>
      print(s"$name? ")
      StdIn.readDouble()
    }
    val tests = Seq(
          "x ^ 3",
          "1 / x",
          "x ^ (1 / y)"
          )
    for (test <- tests) {
      println(s"$test -->")
      val r = for {
        tokens <- lexer(Source.fromString(test).toStream)
        ast <- parser(tokens)
        unopt <- generator(ast)
        opt <- optimizer(unopt)
      } yield opt
      r match {
        case Success(codes) =>
          println("---write---")
          val out = (for (code <- codes) yield code.repr).mkString("\n")
          println(out)
          (for {
            cs <- loader(Source.fromString(out).toStream)
          } yield cs) match {
            case Success(cs) =>
              println("---read---")
              for (c <- cs) println(c.repr)
              println("---answer---")
              (for {
                res <- evaluator(cs)
              } yield res) match {
                case Success(res) =>
                  println(res)
                  println()
                case Failure(e) =>
                  println(e.getMessage)
              }
            case Failure(e) =>
              println(e.getMessage)
          }
        case Failure(e) =>
          println(e.getMessage)
      }
    }
  }
}
