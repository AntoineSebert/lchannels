// Local type for role c:
//    μ(TravelAgency_Proto___c__a_).a⊕{Order(String).a&{Quote(Int).a⊕{Reject(Unit).end, Accept(Unit).a⊕{Address(Int).a&{Date(Int).end}}, Retry(Unit).TravelAgency_Proto___c__a_}}}
package lchannels.examples.travel_agency.protocol.customer

import scala.concurrent.duration.Duration
import lchannels._
import lchannels.examples.travel_agency.protocol.binary

// Input message types for multiparty sessions
case class Quote(p: Int, cont: MPAcceptOrRejectOrRetry)
case class Date(p: Int)

// Output message types for multiparty sessions
case class Order(p: String)
case class Reject(p: Unit)
case class Accept(p: Unit)
case class Retry(p: Unit)
case class Address(p: Int)

// Multiparty session classes
case class MPOrder(a: Out[binary.Order]) {
  def send(v: Order) = {
    val cnt = a !! binary.Order(v.p)_
    MPQuote(cnt)
  }
}
case class MPQuote(a: In[binary.Quote]) {
  def receive(implicit timeout: Duration = Duration.Inf) = {
    a.receive(timeout) match {
      case m @ binary.Quote(p) => Quote(p, MPAcceptOrRejectOrRetry(m.cont))
    }
  }
}
case class MPAcceptOrRejectOrRetry(a: Out[binary.AcceptOrRejectOrRetry]) {
  def send(v: Accept) = {
    val cnt = a !! binary.Accept(v.p)_
    MPAddress(cnt)
  }
  def send(v: Reject) = {
    val cnt = a ! binary.Reject(v.p)
    ()
  }
  def send(v: Retry) = {
    val cnt = a !! binary.Retry(v.p)_
    MPOrder(cnt)
  }
}
case class MPAddress(a: Out[binary.Address]) {
  def send(v: Address) = {
    val cnt = a !! binary.Address(v.p)_
    MPDate(cnt)
  }
}
case class MPDate(a: In[binary.Date]) {
  def receive(implicit timeout: Duration = Duration.Inf) = {
    a.receive(timeout) match {
      case m @ binary.Date(p) => Date(p)
    }
  }
}
