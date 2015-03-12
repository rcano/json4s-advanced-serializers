package json4sadvser

import AdvancedSerializer._
import org.json4s._

object AdvancedSerializerTest extends App {

  case class Foo(a: String, b: Int, c: Option[String], d: Float, oldName: String, someArr: Map[String, Int], someSeq: Seq[String]) {
    def z = 23
  }

  val fooSerializer = new AdvancedSerializer.ForType[Foo] {
    ignoreField(_.a)("Hello")
    renameField(_.b)("bb")
    mapField(_.c)(_ getOrElse null)(Option.apply)
    mapRenameField(_.oldName)("newName")(_.toInt)(_.toString)
  }.build()
  implicit val formats = DefaultFormats + fooSerializer

  val foo = Foo("Hello", 42, None, 42, "21", Map("1" -> 1, "2" -> 2), Seq("a", "b"))
  val ser = fooSerializer.serialize(DefaultFormats)(foo)
  println(ser)
  val newFoo = Extraction.extract[Foo](ser)
  assert(newFoo == foo, s"$foo != $newFoo")
}
