package proteus.internal

import scala.annotation.tailrec

sealed private[proteus] trait Text

private[proteus] object Text {
  case class Line(string: String)    extends Text
  case class Many(texts: List[Text]) extends Text
  case class Indent(text: Text)      extends Text
  case object NewLine                extends Text

  def line(string: String): Text.Line = Text.Line(string)

  def emptyLine: Text.NewLine.type = Text.NewLine

  def many(texts: Text*): Text.Many = Text.Many(texts.toList)

  def many(texts: List[Text]): Text.Many = Text.Many(texts)

  def intersperse(m: Many, sep: Text): Text = {
    @tailrec
    def go(acc: List[Text], rest: List[Text]): List[Text] =
      rest match {
        case head :: tl =>
          if (tl == Nil) {
            go(acc :+ head, tl)
          } else {
            go(acc :+ head :+ sep, tl)
          }
        case Nil        => acc
      }
    Text.many(go(Nil, m.texts))
  }

  def maybe(text: Option[Text]): Text.Many = Text.Many(text.toList)

  def indent(texts: Text*): Text.Indent = indent(texts.toList)

  def indent(texts: List[Text]): Text.Indent = Text.Indent(many(texts))

  def toLines(text: Text): List[String] =

    text match {
      case Line(string) => List(string)
      case Many(texts)  => texts.flatMap(toLines)
      case Indent(text) => toLines(text).map("    " + _)
      case NewLine      => List("")
    }

  def renderText(text: Text): String =
    toLines(text).mkString("\n")
}
