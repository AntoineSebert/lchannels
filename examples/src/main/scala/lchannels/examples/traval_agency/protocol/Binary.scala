// Global type (from /home/osboxes/Desktop/travel-agency.scr)
//    μ(TravelAgency_Proto___c__a_).c->a{Order(String).a->c{Quote(Int).c->a{Reject(Unit).end, Accept(Unit).c->a{Address(Int).a->c{Date(Int).end}}, Retry(Unit).TravelAgency_Proto___c__a_}}}

package lchannels.examples.travel_agency.protocol.binary

import lchannels._

// Classes representing messages (with continuations) in binary sessions
case class Order(p: String)(val cont: Out[Quote])
case class Quote(p: Int)(val cont: Out[AcceptOrRejectOrRetry])

sealed abstract class AcceptOrRejectOrRetry
case class Accept(p: Unit)(val cont: In[Address]) extends AcceptOrRejectOrRetry
case class Address(p: Int)(val cont: Out[Date])
case class Date(p: Int)
case class Reject(p: Unit) extends AcceptOrRejectOrRetry
case class Retry(p: Unit)(val cont: In[Order]) extends AcceptOrRejectOrRetry
