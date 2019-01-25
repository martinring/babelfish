package $for(options.module.scala)$$options.module.scala$$sep$.$endfor$

import scala.scalajs.js

object $name.pascal$ {
$for(type)$
$if(type.plain)$
$for(type.constructor)$
  case class $type.name.pascal$(
$for(type.constructor.parameter)$
      $type.constructor.parameter.name.camel$: $type.constructor.parameter.type$$sep$,
$endfor$

    )
$endfor$
$else$
$if(type.enum)$
  sealed trait $type.name.pascal$
  object $type.name.pascal$ {
$for(type.constructor)$
    case object $type.constructor.name.pascal$ extends $type.name.pascal$
$endfor$
  }
$else$
  sealed trait $type.name.pascal$
  object $type.name.pascal$ {
$for(type.constructor)$
$if(type.constructor.empty)$
    case object $type.constructor.name.pascal$ extends $type.name.pascal$
$else$
    case class $type.constructor.name.pascal$(
$for(type.constructor.parameter)$
        $type.constructor.parameter.name.camel$: $type.constructor.parameter.type$$sep$,
$endfor$

      ) extends $type.name.pascal$
$endif$
$endfor$
  }
$endif$
$endif$
$sep$

$endfor$

  // Codec

  trait Codec[A] {
    def write(from: A): js.Dynamic
    def read(from: js.Dynamic): A
  }

  object Codec {
    def write[A](from: A)(implicit codec: Codec[A]): js.Dynamic = codec.write(from)
    def read[A](from: js.Dynamic)(implicit codec: Codec[A]): A = codec.read(from)
  }

$for(type)$
$if(type.plain)$
$for(type.constructor)$
  implicit object $type.name.pascal$Codec extends Codec[$type.name.pascal$] {
    def write(from: $type.name.pascal$): js.Dynamic = js.Dynamic.literal(
$for(type.constructor.parameter)$
      "$type.constructor.parameter.name.raw$" -> Codec.write(from.$type.constructor.parameter.name.camel$)$sep$,
$endfor$

    )

    def read(from: js.Dynamic): $type.name.pascal$ = match from {
      case from: js.Object => $type.name.pascal$(
$for(type.constructor.parameter)$
        Codec.read[$type.constructor.parameter.type$]("$type.constructor.parameter.name.raw$")$sep$,
$endfor$

      )
    }
  }

$endfor$
$else$
$if(type.enum)$
  implicit object $type.name.pascal$Codec extends Codec[$type.name.pascal$] {
    def write(from: $type.name.pascal$): js.Dynamic = from match {
$for(type.constructor)$
      case $type.name.pascal$.$type.constructor.name.pascal$ => "$type.constructor.name.raw$"
$endfor$
    }
  }
$else$
  implicit object $type.name.pascal$Codec extends Codec[$type.name.pascal$] {
    def write(from: $type.name.pascal$): js.Dynamic = from match {
$for(type.constructor)$
$if(type.constructor.empty)$
      case $type.constructor.name.pascal$ => "$type.constructor.name.raw$"
$else$
      case $type.constructor.name.pascal$($for(type.constructor.parameter)$$type.constructor.parameter.name.camel$$sep$, $endfor$) => js.Dynamic.literal(
$for(type.constructor.parameter)$
        "$type.constructor.parameter.name.raw$" -> Codec.write(from.$type.constructor.parameter.name.camel$)$sep$,
$endfor$      

      )
$endif$    
$endfor$
    }
  }
$endif$
$endif$
$sep$

$endfor$

}