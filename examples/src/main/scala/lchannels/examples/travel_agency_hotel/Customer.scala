package lchannels.examples.travel_agency_hotel.customer

import com.typesafe.scalalogging.StrictLogging
import lchannels._
import lchannels.examples.travel_agency_hotel.protocol.binary
import lchannels.examples.travel_agency_hotel.protocol.customer._

import scala.concurrent.duration.Duration

class Customer(a: Out[binary.Order], h: In[binary.Ok])(implicit timeout: Duration)
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
    val c = MPOrder(a, h) // Wrap the channel in a multiparty session obj
    order(c)

    logInfo("Terminating.")
  }

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
    )
    acceptOrReject(answerChoices) match {
      case "Accept" =>
        val cont = quote.cont.send(Accept())
        logInfo("Quote accepted")

        val details = scala.util.Random.nextInt.abs
        val address = scala.util.Random.nextInt.abs
        logInfo(s"Sending details: '$details'; Sending address: '$address' --- and waiting for date...")

        val date = cont.send(Details1(details)).send(Address(address)).receive
        logInfo(f"Got date: '${date.p}'")

        date.cont.receive
      case "Reject" =>
        quote.cont.send(Reject()).receive
        logInfo("Quote rejected")
    }

    logInfo("Received 'Ok' from Hotel")
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
  private def acceptOrReject(choices: Map[Int, String]): String = {
    println("Which answer should Customer choose?  Please select:")
    for (k <- choices.keys)
      println(f"\t$k - ${choices(k)}")

    print("> ")

    val choice = scala.io.StdIn.readInt
    if (choice >= 1 && choice <= 2)
      choices(choice)
    else {
      println("Invalid choice, please retry")
      acceptOrReject(choices)
    }
  }
}