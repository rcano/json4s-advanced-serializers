package json4sadvser

import AdvancedSerializer._
import org.json4s._

object AdvancedSerializerTest extends App {

  case class Foo(a: String, b: Int, c: Option[String], d: Float, oldName: String) {
    def z = 23
  }
  
  val fooSerializer = new AdvancedSerializer.ForType[Foo] {
    ignoreField(_.a)("Hello")
    renameField(_.b)("bb")
    mapField(_.c)(_ getOrElse "")(Option.apply)
    mapRenameField(_.oldName)("newName")(_.toInt)(_.toString)
  }.build()
  
  fooSerializer.serialize(DefaultFormats)(Foo("hi", 42, None, 42, "old"))
}
