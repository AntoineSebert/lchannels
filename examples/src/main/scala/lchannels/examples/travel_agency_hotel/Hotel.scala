package lchannels.examples.travel_agency_hotel.hotel

import com.typesafe.scalalogging.StrictLogging
import lchannels._
import lchannels.examples.travel_agency_hotel.protocol.binary
import lchannels.examples.travel_agency_hotel.protocol.hotel._

import scala.concurrent.duration.Duration

class Hotel(a: In[binary.CancelOrDetails2], c: Out[binary.Ok])(implicit timeout: Duration)
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
    val cod = MPCancelOrDetails2(a, c) // Wrap the channel in a multiparty session obj
    handleReservation(cod)
    logInfo("Terminating.")
  }

  private def handleReservation(c: MPCancelOrDetails2): Unit = {
    logInfo("Waiting for reservation...")

    c.receive match {
      case Details2(p, cont) => {
        logInfo(f"Received details: '$p'")
        
        logInfo(f"Sending Ok...")
        cont.send(Ok())
      }
      case Cancel((), cont) => {
        logInfo(f"Cancelled")

        logInfo(f"Sending Ok...")
        cont.send(Ok())
      }
    }
  }
}