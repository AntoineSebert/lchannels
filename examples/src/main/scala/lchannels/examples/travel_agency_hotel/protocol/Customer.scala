// Local type for role Customer:
//    Agency⊕{Order(String).Agency&{Quote(Int).Agency⊕{Reject(Unit).Hotel&{Ok(Unit).end}, Accept(Unit).Agency⊕{Details1(Int).Agency⊕{Address(Int).Agency&{Date(Int).Hotel&{Ok(Unit).end}}}}}}}
package lchannels.examples.travel_agency_hotel.protocol.customer

import lchannels.examples.travel_agency_hotel.protocol.binary
import scala.concurrent.duration.Duration
import lchannels._

// Input message types for multiparty sessions
case class Quote(p: Int, cont: MPAcceptOrReject)
case class Ok(p: Unit)
case class Date(p: Int, cont: MPOk)

// Output message types for multiparty sessions
case class Order(p: String)
case class Reject(p: Unit)
case class Accept(p: Unit)
case class Details1(p: Int)
case class Address(p: Int)

// Multiparty session classes
case class MPOrder(Agency: Out[binary.Order], Hotel: In[binary.Ok]) {
  def send(v: Order) = {
    val cnt = Agency !! binary.Order(v.p)_
    MPQuote(cnt, Hotel)
  }
}
case class MPQuote(Agency: In[binary.Quote], Hotel: In[binary.Ok]) {
  def receive(implicit timeout: Duration = Duration.Inf) = {
    Agency.receive(timeout) match {
      case m @ binary.Quote(p) => Quote(p, MPAcceptOrReject(m.cont, Hotel))
    }
  }
}
case class MPAcceptOrReject(Agency: Out[binary.AcceptOrReject], Hotel: In[binary.Ok]) {
  def send(v: Accept) = {
    val cnt = Agency !! binary.Accept(v.p)_
    MPDetails1(cnt, Hotel)
  }
  def send(v: Reject) = {
    val cnt = Agency ! binary.Reject(v.p)
    MPOk(cnt, Hotel)
  }
}
case class MPDetails1(Agency: Out[binary.Details1], Hotel: In[binary.Ok]) {
  def send(v: Details1) = {
    val cnt = Agency !! binary.Details1(v.p)_
    MPAddress(cnt, Hotel)
  }
}
case class MPAddress(Agency: Out[binary.Address], Hotel: In[binary.Ok]) {
  def send(v: Address) = {
    val cnt = Agency !! binary.Address(v.p)_
    MPDate(cnt, Hotel)
  }
}
case class MPDate(Agency: In[binary.Date], Hotel: In[binary.Ok]) {
  def receive(implicit timeout: Duration = Duration.Inf) = {
    Agency.receive(timeout) match {
      case m @ binary.Date(p) => Date(p, MPOk((), Hotel))
    }
  }
}
case class MPOk(Agency: Unit, Hotel: In[binary.Ok]) {
  def receive(implicit timeout: Duration = Duration.Inf) = {
    Hotel.receive(timeout) match {
      case m @ binary.Ok(p) => Ok(p)
    }
  }
}
