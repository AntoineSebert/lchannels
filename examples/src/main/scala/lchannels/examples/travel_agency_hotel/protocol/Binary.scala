// Global type (from /home/osboxes/Desktop/travel-agency-hotel.scr)
//    Customer->Agency{Order(String).Agency->Customer{Quote(Int).Customer->Agency{Reject(Unit).Agency->Hotel{Cancel(Unit).Hotel->Customer{Ok(Unit).end}}, Accept(Unit).Customer->Agency{Details1(Int).Agency->Hotel{Details2(Int).Customer->Agency{Address(Int).Agency->Customer{Date(Int).Hotel->Customer{Ok(Unit).end}}}}}}}}

package lchannels.examples.travel_agency_hotel.protocol.binary

import lchannels._

case class Order(p: String)(val cont: Out[Quote])
case class Quote(p: Int)(val cont: Out[AcceptOrReject])
sealed abstract class AcceptOrReject
case class Accept(p: Unit)(val cont: In[Details1]) extends AcceptOrReject
case class Details1(p: Int)(val cont: In[Address])
case class Address(p: Int)(val cont: Out[Date])
case class Date(p: Int)
case class Reject(p: Unit) extends AcceptOrReject
sealed abstract class CancelOrDetails2
case class Cancel(p: Unit) extends CancelOrDetails2
case class Details2(p: Int) extends CancelOrDetails2
case class Ok(p: Unit)

