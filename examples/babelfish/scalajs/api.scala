/*
 * THIS FILE IS AUTO GENERATED BY BABELFISH. YOU SHOULD NOT MODIFY IT!
 */

package $for(options.module)$$options.module$$sep$.$endfor$

import scala.scalajs.js
import scala.concurrent.{ Future, Promise }
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

import $name.pascal$._

trait $api.name.pascal$ {
$for(api.method)$
  def $api.method.name.camel$($for(api.method.parameter)$$api.method.parameter.name.camel$: $api.method.parameter.type$$sep$, $endfor$): $if(api.method.return)$Future[$api.method.return$]$else$Unit$endif$
$endfor$
}

$if(api.local)$object $api.name.pascal$ {$else$$if(api.remote)$object $api.name.pascal$ {$endif$$endif$
$if(api.remote)$
  def remote(send: js.Any => Unit): (js.Any => Unit, $api.name.pascal$) = {
    var id = 0
    var handlers = mutable.Map.empty[Int,Promise[js.Any]]
    def nextId() = { id += 1; id }    
    val impl = new $api.name.pascal$ {
$for(api.method)$
      override def $api.method.name.camel$($for(api.method.parameter)$$api.method.parameter.name.camel$: $api.method.parameter.type$$sep$, $endfor$): $if(api.method.return)$Future[$api.method.return$]$else$Unit$endif$ = {
$if(api.method.return)$
        val id = nextId()
$endif$
        val msg = js.Dynamic.literal(
          "jsonrpc" -> "2.0",
          "method" -> "$api.method.name.raw$",
          "params" -> js.Dynamic.literal(
$for(api.method.parameter)$
            "$api.method.parameter.name.raw$" -> Codec.write($api.method.parameter.name.camel$)$sep$,
$endfor$            

          )$if(api.method.return)$,
          "id" -> id$endif$
        )
        send(msg)
$if(api.method.return)$
        val handler = Promise[js.Any]
        handlers += id -> handler
        handler.future.map(Codec.read[$api.method.return$])
$endif$        
      }
$endfor$      
    }
    def receive(msg: js.Any): Unit = {
      val d = msg.asInstanceOf[js.Dictionary[js.Any]]
      for {
        id <- d.get("id").asInstanceOf[Option[Int]]
        result <- d.get("result")
        handler <- handlers.remove(id)
      } handler.success(result)     
    }
    (receive,impl)
  }
$if(api.local)$

$endif$
$endif$  
$if(api.local)$
  def local(send: js.Any => Unit)(impl: $api.name.pascal$): js.Any => Unit = { msg: js.Any =>    
    val d = msg.asInstanceOf[js.Dictionary[js.Any]]
    d.get("method").asInstanceOf[Option[String]].foreach {
$for(api.method)$      
      case "$api.method.name.raw$" => 
        $if(api.method.return)$val result = $endif$for {
          params <- d.get("params").asInstanceOf[Option[js.Dictionary[js.Any]]]
$for(api.method.parameter)$
          $api.method.parameter.name.camel$ <- params.get("$api.method.parameter.name.raw$")
$endfor$
        } $if(api.method.return)$yield $endif$impl.$api.method.name.camel$($for(api.method.parameter)$Codec.read[$api.method.parameter.type$]($api.method.parameter.name.camel$)$sep$, $endfor$)
$if(api.method.return)$
        result.foreach(res => res.foreach(res => send(Codec.write(res))))
$endif$
$endfor$      
    }
  }
$endif$
$if(api.local)$}$else$$if(api.remote)$}$endif$$endif$