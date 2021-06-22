package lchannels.examples.travel_agency_hotel.agency

import com.typesafe.scalalogging.StrictLogging
import lchannels._
import lchannels.examples.travel_agency_hotel.protocol.binary
import lchannels.examples.travel_agency_hotel.protocol.agency._

import scala.concurrent.duration.Duration

class Agency(c: In[binary.Order], h: Out[binary.CancelOrDetails2])(implicit timeout: Duration)
  extends Runnable with StrictLogging {
  private def logTrace(msg: String) = logger.trace(msg)
  private def logDebug(msg: String) = logger.debug(msg)
  private def logInfo(msg: String) = logger.info(msg)
  private def logWarn(msg: String) = logger.warn(msg)
  private def logError(msg: String) = logger.error(msg)

  // Own thread
  private val thread = { val t = new Thread(this); t.start(); t }
  def join() = thread.join()

  override def run() = {
    val o = MPOrder(c, h) // Wrap the channel in a multiparty session obj
    handleOrder(o)
    logInfo("Terminating.")
  }

  private def handleOrder(c: MPOrder): Unit = {
    logInfo("Waiting for order...")
    val order = c.receive

    logInfo(f"Received order: '${order.p}'")
    // replace by enum ?
    val quote = order.p match {
      case "Hawaii" => 10
      case "France" => 100
      case "Akallabêth" => 150
      case "Denmark" => 200
      case _ => 1000 // We can find any book, but it will be expensive...
    }

    logInfo(f"Sending quote: $quote --- then waiting for answer...")
    order.cont.send(Quote(quote)).receive match {
      case Accept((), cont) => {
        logInfo("Waiting for details...")
        val details = cont.receive
        logInfo(f"Received details: '${details.p}'")

        logInfo(f"Forwarding details: '${details.p}' --- and waiting for address...")
        val address = details.cont.send(Details2(details.p)).receive
        logInfo(f"Received address: '${address.p}'")

        val date = scala.util.Random.nextInt.abs
        logInfo(f"Sending date: $date...")
        address.cont.send(Date(date))
      }
      case Reject((), cont) => {
        logInfo("Quote rejected --- cancelling...")
        cont.send(Cancel())
      }
    }
  }
}
