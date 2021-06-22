// Local type for role a:
//    μ(TravelAgency_Proto___c__a_).c&{Order(String).c⊕{Quote(Int).c&{Reject(Unit).end, Accept(Unit).c&{Address(Int).c⊕{Date(Int).end}}, Retry(Unit).TravelAgency_Proto___c__a_}}}
package lchannels.examples.travel_agency.protocol.agency

import lchannels.examples.travel_agency.protocol.binary
import scala.concurrent.duration.Duration
import lchannels._

// Input message types for multiparty sessions
case class Order(p: String, cont: MPQuote)
sealed abstract class MsgMPAcceptOrRejectOrRetry
case class Reject(p: Unit) extends MsgMPAcceptOrRejectOrRetry
case class Accept(p: Unit, cont: MPAddress) extends MsgMPAcceptOrRejectOrRetry
case class Retry(p: Unit, cont: MPOrder) extends MsgMPAcceptOrRejectOrRetry
case class Address(p: Int, cont: MPDate)

// Output message types for multiparty sessions
case class Quote(p: Int)
case class Date(p: Int)

// Multiparty session classes
case class MPOrder(c: In[binary.Order]) {
  def receive(implicit timeout: Duration = Duration.Inf) = {
    c.receive(timeout) match {
      case m @ binary.Order(p) => Order(p, MPQuote(m.cont))
    }
  }
}
case class MPQuote(c: Out[binary.Quote]) {
  def send(v: Quote) = {
    val cnt = c !! binary.Quote(v.p)_
    MPAcceptOrRejectOrRetry(cnt)
  }
}
case class MPAcceptOrRejectOrRetry(c: In[binary.AcceptOrRejectOrRetry]) {
  def receive(implicit timeout: Duration = Duration.Inf) = {
    c.receive(timeout) match {
      case m @ binary.Accept(p) => Accept(p, MPAddress(m.cont))
      case m @ binary.Reject(p) => Reject(p)
      case m @ binary.Retry(p) => Retry(p, MPOrder(m.cont))
    }
  }
}
case class MPAddress(c: In[binary.Address]) {
  def receive(implicit timeout: Duration = Duration.Inf) = {
    c.receive(timeout) match {
      case m @ binary.Address(p) => Address(p, MPDate(m.cont))
    }
  }
}
case class MPDate(c: Out[binary.Date]) {
  def send(v: Date) = {
    val cnt = c ! binary.Date(v.p)
    ()
  }
}
