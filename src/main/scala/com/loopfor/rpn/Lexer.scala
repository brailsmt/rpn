package com.loopfor.rpn

import scala.annotation.tailrec
import scala.collection.immutable.Stream
import scala.util.Try

/**
 * A lexical analyzer that transforms a stream of characters into a stream of tokens.
 * 
 * Tokens must either be delimited by one or more whitespace characters, or be clearly
 * distinguishable from each other if not separated by whitespace.
 */
trait Lexer {
  def apply(in: Stream[Char]): Try[Stream[Token]]
}

private class BasicLexer extends Lexer {
  def apply(in: Stream[Char]): Try[Stream[Token]] = Try {
    def tokens(in: Stream[Char]): Stream[Token] = tokenize(in) match {
      case (EOSToken, _) => Stream.Empty
      case (token, rest) => token #:: tokens(rest)
    }
    tokens(in)
  }

  @tailrec private def tokenize(in: Stream[Char]): (Token, Stream[Char]) = in.headOption match {
    case Some(Space(_)) => tokenize(in.tail)
    case Some(Token(t)) => (t, in.tail)
    case Some(Digit(c)) => readNumber(in.tail, s"$c")
    case Some(Letter(c)) => readSymbol(in.tail, s"$c")
    case Some(c) => throw new Exception(s"$c: unrecognized character")
    case None => (EOSToken, in)
  }

  @tailrec private def readNumber(in: Stream[Char], lexeme: String): (Token, Stream[Char]) = in.headOption match {
    case Some('.') =>
      if (lexeme contains '.') throw new Exception(s"$lexeme.: malformed number")
      else readNumber(in.tail, lexeme + '.')
    case Some(Digit(c)) => readNumber(in.tail, lexeme + c)
    case _ =>
      if (lexeme.last == '.') throw new Exception(s"$lexeme: malformed number")
      else (NumberToken(lexeme), in)
  }

  @tailrec private def readSymbol(in: Stream[Char], lexeme: String): (Token, Stream[Char]) = in.headOption match {
    case Some(Letter(c)) => readSymbol(in.tail, lexeme + c)
    case _ =>
      (lexeme match {
        case Token(t) => t
        case _ => SymbolToken(lexeme)
      }, in)
  }

  private object Digit {
    private val digits = ('0' to '9').to[Set]
    def unapply(c: Char): Option[Char] = if (digits(c)) Some(c) else None
  }

  private object Letter {
    private val letters = (('A' to 'Z') ++ ('a' to 'z')).to[Set]
    def unapply(c: Char): Option[Char] = if (letters(c)) Some(c) else None
  }

  private object Space {
    private val whitespace: Set[Char] = Set(' ', '\n', '\r', '\t', '\f')
    def unapply(c: Char): Option[Char] = if (whitespace(c)) Some(c) else None
  }
}

object BasicLexer {
  def apply(): Lexer = new BasicLexer
  def apply(in: Stream[Char]): Try[Stream[Token]] = apply()(in)
}
