/*
 * THIS FILE IS AUTO GENERATED BY BABELFISH. YOU SHOULD NOT MODIFY IT!
 */

package de.dfki

import scala.scalajs.js

object Messages {
  case class Person(
      name: String,
      age: Int
    )

  sealed trait Severity
  object Severity {
    case object Info extends Severity
    case object Warning extends Severity
    case object Error extends Severity
  }

  sealed trait ListThing
  object ListThing {
    case object Empty extends ListThing
    case class Cons(
        head: Int,
        tail: ListThing
      ) extends ListThing
  }

  // Codec

  trait Codec[A] {
    def write(from: A): js.Any
    def read(from: js.Any): A
  }

  object Codec {
    def write[A](from: A)(implicit codec: Codec[A]): js.Any = codec.write(from)
    def read[A](from: js.Any)(implicit codec: Codec[A]): A = codec.read(from)
    def apply[A](w: A => js.Any, r: js.Any => A): Codec[A] = new Codec[A] {      
      def write(from: A): js.Any = w(from)
      def read(from: js.Any): A = r(from)
    }
    implicit val StringCodec: Codec[String] = Codec(s => s, _.asInstanceOf[String])
    implicit val IntCodec: Codec[Int] = Codec(s => s, _.asInstanceOf[Int])
    implicit val BooleanCodec: Codec[Boolean] = Codec(s => s, _.asInstanceOf[Boolean])    
  }


  implicit object PersonCodec extends Codec[Person] {
    def write(from: Person): js.Any = js.Dynamic.literal(
      "name" -> Codec.write(from.name),
      "age" -> Codec.write(from.age)
    )

    def read(from: js.Any): Person = from match {
      case from: js.Object => Person(
        Codec.read[String](from.asInstanceOf[js.Dictionary[js.Any]]("name")),
        Codec.read[Int](from.asInstanceOf[js.Dictionary[js.Any]]("age"))
      )
    }
  }


  implicit object SeverityCodec extends Codec[Severity] {
    def write(from: Severity): js.Any = from match {
      case Severity.Info => "info"
      case Severity.Warning => "warning"
      case Severity.Error => "error"
    }

    def read(from: js.Any): Severity =       
      if (from.asInstanceOf[String] == "info") Severity.Info
      else if (from.asInstanceOf[String] == "warning") Severity.Warning
      else if (from.asInstanceOf[String] == "error") Severity.Error
      else sys.error("invalid severity")    
  }

  implicit object ListThingCodec extends Codec[ListThing] {
    def write(from: ListThing): js.Any = from match {
      case ListThing.Empty => "empty"
      case ListThing.Cons(head,tail) => js.Dynamic.literal(
        "type" -> "cons",
        "head" -> Codec.write(head),
        "tail" -> Codec.write(tail)
      )
    }

    def read(from: js.Any): ListThing = {
      val from_ = from.asInstanceOf[js.Dictionary[js.Any]]
      if (from.asInstanceOf[String] == "empty") ListThing.Empty
      else if (from_("type").asInstanceOf[String] == "cons") ListThing.Cons(
        Codec.read[Int](from_("head")),
        Codec.read[ListThing](from_("tail"))
      )
      else sys.error("invalid list thing")
    }  
  }

}