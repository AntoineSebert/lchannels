// Local type for role Agency:
//    Customer&{Order(String).Customer⊕{Quote(Int).Customer&{Reject(Unit).Hotel⊕{Cancel(Unit).end}, Accept(Unit).Customer&{Details1(Int).Hotel⊕{Details2(Int).Customer&{Address(Int).Customer⊕{Date(Int).end}}}}}}}
package lchannels.examples.travel_agency_hotel.protocol.agency

import lchannels.examples.travel_agency_hotel.protocol.binary

import scala.concurrent.duration.Duration
import lchannels._

// Input message types for multiparty sessions
case class Order(p: String, cont: MPQuote)
sealed abstract class MsgMPAcceptOrReject
case class Reject(p: Unit, cont: MPCancel) extends MsgMPAcceptOrReject
case class Accept(p: Unit, cont: MPDetails1) extends MsgMPAcceptOrReject
case class Details1(p: Int, cont: MPDetails2)
case class Address(p: Int, cont: MPDate)

// Output message types for multiparty sessions
case class Quote(p: Int)
case class Cancel(p: Unit)
case class Details2(p: Int)
case class Date(p: Int)

// Multiparty session classes
case class MPOrder(Customer: In[binary.Order], Hotel: Out[binary.CancelOrDetails2]) {
  def receive(implicit timeout: Duration = Duration.Inf): Order = {
    Customer.receive(timeout) match {
      case m @ binary.Order(p) => Order(p, MPQuote(m.cont, Hotel))
    }
  }
}
case class MPQuote(Customer: Out[binary.Quote], Hotel: Out[binary.CancelOrDetails2]) {
  def send(v: Quote): MPAcceptOrReject = {
    val cnt = Customer !! binary.Quote(v.p)_
    MPAcceptOrReject(cnt, Hotel)
  }
}
case class MPAcceptOrReject(Customer: In[binary.AcceptOrReject], Hotel: Out[binary.CancelOrDetails2]) {
  def receive(implicit timeout: Duration = Duration.Inf): MsgMPAcceptOrReject = {
    Customer.receive(timeout) match {
      case m @ binary.Accept(p) => Accept(p, MPDetails1(m.cont, Hotel))
      case m @ binary.Reject(p) => Reject(p, MPCancel((), Hotel))
    }
  }
}
case class MPDetails1(Customer: In[binary.Details1], Hotel: Out[binary.CancelOrDetails2]) {
  def receive(implicit timeout: Duration = Duration.Inf): Details1 = {
    Customer.receive(timeout) match {
      case m @ binary.Details1(p) => Details1(p, MPDetails2(m.cont, Hotel))
    }
  }
}
case class MPDetails2(Customer: In[binary.Address], Hotel: Out[binary.CancelOrDetails2]) {
  def send(v: Details2): MPAddress = {
    val cnt: Unit = Hotel ! binary.Details2(v.p)
    MPAddress(Customer, cnt)
  }
}
case class MPAddress(Customer: In[binary.Address], Hotel: Unit) {
  def receive(implicit timeout: Duration = Duration.Inf): Address = {
    Customer.receive(timeout) match {
      case m @ binary.Address(p) => Address(p, MPDate(m.cont, Hotel))
    }
  }
}
case class MPDate(Customer: Out[binary.Date], Hotel: Unit) {
  def send(v: Date): Unit = {
    val cnt: Unit = Customer ! binary.Date(v.p)
    ()
  }
}
case class MPCancel(Customer: Unit, Hotel: Out[binary.CancelOrDetails2]) {
  def send(v: Cancel): Unit = {
    val cnt: Unit = Hotel ! binary.Cancel(v.p)
    ()
  }
}