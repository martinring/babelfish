/*
 * THIS FILE IS AUTO GENERATED BY BABELFISH. YOU SHOULD NOT MODIFY IT!
 */

package $for(options.module)$$options.module$$sep$.$endfor$.$name.pascal$

import scala.scalajs.js

trait $name.pascal$ {
$for(method)$
  def $method.name.camel$($for(method.parameter)$$method.parameter.name.camel$: $method.parameter.type$$sep$, $endfor$): $if(method.return)$Future[$method.return$]$else$Unit$endif$
$endfor$
}

object $name.pascal$ {  
  def remote(send: js.Any => Unit): (js.Any => Unit, $name.pascal$) = {
    var id = 0
    def nextId() = { id += 1; id }
    val impl = new $name.pascal$ {
$for(method)$
      override def $method.name.camel$($for(method.parameter)$$method.parameter.name.camel$: $method.parameter.type$$sep$, $endfor$): $if(method.return)$Future[$method.return$]$else$Unit$endif$ = {
        val msg = js.Dynamic.literal(
          "method" -> "$method.name.raw$",
          "params" -> js.Dynamic.literal(
$for(method.parameter)$
            "$method.parameter.name.raw$" -> Codec.write($method.parameter.name.camel$)$sep$,
$endfor$            

          ),
          "id" -> nextId()
        )
        send(msg)
      }
$endfor$      
    }
  }

  def local(send: js.Any => Unit)(implementation: $name.pascal$): js.Any => Unit = { msg =>
    
  }
}