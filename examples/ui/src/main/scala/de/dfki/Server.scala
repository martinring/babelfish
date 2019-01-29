/*
 * THIS FILE IS AUTO GENERATED BY BABELFISH. YOU SHOULD NOT MODIFY IT!
 */

package de.dfki

import scala.scalajs.js
import scala.concurrent.{ Future, Promise }
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

import Messages._

trait Server {
  def register(person: Person): Future[Boolean]
  def chat(message: String): Unit
}

object Server {
  def remote(send: js.Any => Unit): (js.Any => Unit, Server) = {
    var id = 0
    var handlers = mutable.Map.empty[Int,Promise[js.Any]]
    def nextId() = { id += 1; id }    
    val impl = new Server {
      override def register(person: Person): Future[Boolean] = {
        val id = nextId()
        val msg = js.Dynamic.literal(
          "jsonrpc" -> "2.0",
          "method" -> "register",
          "params" -> js.Dynamic.literal(
            "person" -> Codec.write(person)
          ),
          "id" -> id
        )
        send(msg)
        val handler = Promise[js.Any]
        handlers += id -> handler
        handler.future.map(Codec.read[Boolean])
      }
      override def chat(message: String): Unit = {
        val msg = js.Dynamic.literal(
          "jsonrpc" -> "2.0",
          "method" -> "chat",
          "params" -> js.Dynamic.literal(
            "message" -> Codec.write(message)
          )
        )
        send(msg)
      }
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

  def local(send: js.Any => Unit)(impl: Server): js.Any => Unit = { msg: js.Any =>    
    val d = msg.asInstanceOf[js.Dictionary[js.Any]]
    d.get("method").asInstanceOf[Option[String]].foreach {
      case "register" => 
        val result = for {
          params <- d.get("params").asInstanceOf[Option[js.Dictionary[js.Any]]]
          person <- params.get("person")
        } yield impl.register(Codec.read[Person](person))
        result.foreach(res => res.foreach(res => send(Codec.write(res))))
      case "chat" => 
        for {
          params <- d.get("params").asInstanceOf[Option[js.Dictionary[js.Any]]]
          message <- params.get("message")
        } impl.chat(Codec.read[String](message))
    }
  }
}