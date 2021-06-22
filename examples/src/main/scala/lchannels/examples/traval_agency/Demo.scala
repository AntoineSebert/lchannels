package lchannels.examples.traval_agency.demo

import lchannels._
import lchannels.examples.travel_agency.protocol.binary.Order
import lchannels.examples.travel_agency.agency.Agency
import lchannels.examples.travel_agency.customer.Customer

object Local extends App {
  // Helper method to ease external invocation
  def run() = main(Array())

  Demo.start()
}

object Demo {
  def start() = {
    import scala.concurrent.duration._
    implicit val timeout = 60.seconds

    val (ca, sa) = LocalChannel.factory[Order]()

    val agency = new Agency(ca)
    val customer = new Customer(sa)

    agency.join()
    customer.join()
  }
}
