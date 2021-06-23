package lchannels.examples.travel_agency.customer

import com.typesafe.scalalogging.StrictLogging
import lchannels._
import lchannels.examples.travel_agency.protocol.binary
import lchannels.examples.travel_agency.protocol.customer._

import scala.concurrent.duration.Duration

class Customer(s: Out[binary.Order])(implicit timeout: Duration)
  extends Runnable with StrictLogging {
  private def logTrace(msg: String): Unit = logger.trace(msg)
  private def logDebug(msg: String): Unit = logger.debug(msg)
  private def logInfo(msg: String): Unit = logger.info(msg)
  private def logWarn(msg: String): Unit = logger.warn(msg)
  private def logError(msg: String): Unit = logger.error(msg)

  // Own thread
  private val thread = { val t = new Thread(this); t.start(); t }
  def join(): Unit = thread.join()

  override def run(): Unit = {
    val c = MPOrder(s) // Wrap the channel in a multiparty session obj
    order(c)

    logInfo("Terminating.")
  }

  @scala.annotation.tailrec
  private def order(c: MPOrder): Unit = {
    logInfo("Started.  Waiting for multiparty session...")
    // replace by enum ?
    val placeChoices = Map(
      1 -> "Hawaii",
      2 -> "France",
      3 -> "Akallabêth",
      4 -> "Denmark",
    )
    val location = chooseLocation(placeChoices)
    logInfo(f"Sending location: '$location' --- and waiting for quote...")
    val quote = c.send(Order(location)).receive
    logInfo(f"Got quote: ${quote.p}")

    val answerChoices = Map(
      1 -> "Accept",
      2 -> "Reject",
      3 -> "Retry",
    )
    acceptOrRejectOrRetry(answerChoices) match {
      case "Accept" =>
        logInfo("Quote accepted")
        val address = scala.util.Random.nextInt.abs

        logInfo(f"Sending address: '$address' --- and waiting for date...")
        val date = quote.cont.send(Accept()).send(Address(address)).receive
        logInfo(f"Got date: ${date.p}")
      case "Reject" =>
        logInfo("Quote rejected")
        quote.cont.send(Reject())
      case "Retry" =>
        logInfo("retry")
        order(quote.cont.send(Retry()))
    }
  }

  @scala.annotation.tailrec
  private def chooseLocation(choices: Map[Int, String]): String = {
    println("Which location should Customer choose?  Please select:")
    for (k <- choices.keys)
      println(f"\t$k - ${choices(k)}")

    print("> ")

    val choice = scala.io.StdIn.readInt
    if (choice >= 1 && choice <= 4)
      choices(choice)
    else {
      println("Invalid choice, please retry")
      chooseLocation(choices)
    }
  }

  @scala.annotation.tailrec
  private def acceptOrRejectOrRetry(choices: Map[Int, String]): String = {
    println("Which answer should Customer choose?  Please select:")
    for (k <- choices.keys)
      println(f"\t$k - ${choices(k)}")

    print("> ")

    val choice = scala.io.StdIn.readInt
    if (choice >= 1 && choice <= 3)
      choices(choice)
    else {
      println("Invalid choice, please retry")
      acceptOrRejectOrRetry(choices)
    }
  }
}