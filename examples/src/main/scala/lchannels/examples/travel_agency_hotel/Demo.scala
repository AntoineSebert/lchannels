package lchannels.examples.travel_agency_hotel.demo

import lchannels._
import lchannels.examples.travel_agency_hotel.protocol.binary._
import lchannels.examples.travel_agency_hotel.agency.Agency
import lchannels.examples.travel_agency_hotel.customer.Customer
import lchannels.examples.travel_agency_hotel.hotel.Hotel

object Local extends App {
  // Helper method to ease external invocation
  def run(): Unit = main(Array())

  Demo.start()
}

object Demo {
  def start(): Unit = {
    import scala.concurrent.duration._
    implicit val timeout: FiniteDuration = 60.seconds

    val (ca, sa) = LocalChannel.factory[Order]()
    val (cb, sb) = LocalChannel.factory[CancelOrDetails2]()
    val (cc, sc) = LocalChannel.factory[Ok]()

    val agency = new Agency(ca, sb)
    val customer = new Customer(sa, cc)
    val hotel = new Hotel(cb, sc)

    agency.join()
    customer.join()
    hotel.join()
  }
}
