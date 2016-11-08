// lchannels - session programming in Scala
// Copyright (c) 2016, Alceste Scalas and Imperial College London
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
/** @author Alceste Scalas <alceste.scalas@imperial.ac.uk> */
package lchannels.examples.game.a

import lchannels._
import lchannels.examples.game.protocol.binary
import lchannels.examples.game.protocol.a._

import scala.concurrent.duration._

import com.typesafe.scalalogging.StrictLogging
  
class Client(name: String, s: In[binary.PlayA], wait: Duration)
            (implicit timeout: Duration)
    extends Runnable with StrictLogging {
  private def logTrace(msg: String) = logger.trace(f"${name}: ${msg}")
  private def logDebug(msg: String) = logger.debug(f"${name}: ${msg}")
  private def logInfo(msg: String) = logger.info(f"${name}: ${msg}")
  private def logWarn(msg: String) = logger.warn(f"${name}: ${msg}")
  private def logError(msg: String) = logger.error(f"${name}: ${msg}")

  // Own thread
  private val thread = { val t = new Thread(this); t.start(); t }
  def join() = thread.join()
  
  override def run() = {
    val c = MPPlayA(s) // Wrap the channel in a multiparty session obj
    
    logInfo("Started.  Waiting for multiparty session...")
    val game = c.receive().p
    logInfo("...done.  Waiting for info...")
    val info = game.receive()
    logInfo(f"...got '${info.p}'.  Sending info to B...")
    val gloop = info.cont.send(InfoAB(info.p + f", ${name}"))
    logInfo("...done.  Starting game loop.")
    loop(gloop, 1)
  }
    
  @scala.annotation.tailrec
  private def loop(g: MPMov1ABOrMov2AB, loopn: Int): Unit = {
	  logInfo(f"Delay: ${wait}")
	  Thread.sleep(wait.toMillis)
	  logInfo(f"Sending Mov1AB(${loopn}) to B, and waiting C's move")
	  g.send(Mov1AB(loopn)).receive() match {
	    case Mov1CA(p, cont) => {
		    logInfo(f"Got Mov1CA(${p}), sending Mov2AB(true)")
  		  val g2 = cont.send(Mov2AB(true))
		    g2.receive() match {
		      case Mov1CA(p, cont) => {
  			    logInfo(f"Got Mov1CA(${p}), looping")
	  		    loop(cont, loopn+1)
		      }
		      case Mov2CA(p, cont) => {
			      logInfo(f"Got Mov2CA(${p}), looping")
			      loop(cont, loopn+1)
		      }
		    }
	    }
	    case Mov2CA(p, cont) => {
		    logInfo(f"Got Mov1CA(${p}), sending Mov1AB(${loopn+1})")
		    val g2 = cont.send(Mov1AB(loopn+1))
		    g2.receive() match {
		      case Mov1CA(p, cont) => {
			      logInfo(f"Got Mov1CA(${p}), looping")
			      loop(cont, loopn+2)
		      }
		      case Mov2CA(p, cont) => {
			      logInfo(f"Got Mov2CA(${p}), looping")
			      loop(cont, loopn+2)
		      }
		    }
	    }
	  }
  }
}

object Actor extends App {
  // Helper method to ease external invocation
  def run() = main(Array())
  
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import com.typesafe.config.ConfigFactory
  import akka.actor.ActorSystem
  
  import binary.actor.{ConnectA => Connect}
  
  val config = ConfigFactory.load() // Loads resources/application.conf
  implicit val as = ActorSystem("GameClientASys",
                          config = Some(config.getConfig("GameClientASys")),
                          defaultExecutionContext = Some(global))
  
  ActorChannel.setDefaultEC(global)
  ActorChannel.setDefaultAS(as)
  
  implicit val timeout = 30.seconds
  
  val serverPath =  "akka.tcp://GameServerSys@127.0.0.1:31340/user/a"
  println(f"[*] Connecting to ${serverPath}...")
  val c: Out[Connect] = ActorOut[Connect](serverPath)
  val c2 = c !! Connect()_
  
  val client = new Client("Alice", c2, 3.seconds)
  
  client.join()
  as.terminate()
}
