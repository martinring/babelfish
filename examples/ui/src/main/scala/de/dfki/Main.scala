import de.dfki.Messages.Person
import de.dfki.Server
import org.scalajs.dom._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSON

object Main {
  def main(args: Array[String]): Unit = {
    val socket = new WebSocket("ws://localhost:8000")
    socket.addEventListener("open",(e: Event) => {
      val send = { (msg: js.Any) =>
        val str = JSON.stringify(msg)
        println("sending: " + str)
        socket.send(str)
      }
      val (remoteSink,remote) = Server.remote(send)
      val localSink = Server.local(send)(new Server {
        override def register(person: Person): Future[Boolean] = Future.successful(person.age > 18)
        override def chat(message: String): Unit = {
          val div = document.createElement("div")
          document.body.appendChild(div)
          div.innerHTML = message
        }
      })
      socket.addEventListener("message",(e: MessageEvent) => {
        val msg = JSON.parse(e.data.asInstanceOf[String])
        println("received: " + e.data)
        remoteSink(msg)
        localSink(msg)
      })
      remote.chat("HALLOOOOO")
      remote.register(Person("Martin",14)).foreach(println)
    })
  }
}