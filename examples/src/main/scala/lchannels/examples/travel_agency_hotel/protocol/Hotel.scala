// Local type for role Hotel:
//    Agency&{Details2(Int).Customer⊕{Ok(Unit).end}, Cancel(Unit).Customer⊕{Ok(Unit).end}}
package lchannels.examples.travel_agency_hotel.protocol.hotel

import lchannels.examples.travel_agency_hotel.protocol.binary
import scala.concurrent.duration.Duration
import lchannels._

// Input message types for multiparty sessions
sealed abstract class MsgMPCancelOrDetails2
case class Details2(p: Int, cont: MPOk) extends MsgMPCancelOrDetails2
case class Cancel(p: Unit, cont: MPOk) extends MsgMPCancelOrDetails2

// Output message types for multiparty sessions
case class Ok(p: Unit)

// Multiparty session classes
case class MPCancelOrDetails2(Agency: In[binary.CancelOrDetails2], Customer: Out[binary.Ok]) {
  def receive(implicit timeout: Duration = Duration.Inf): MsgMPCancelOrDetails2 = {
    Agency.receive(timeout) match {
      case m @ binary.Cancel(p) => Cancel(p, MPOk((), Customer))
      case m @ binary.Details2(p) => Details2(p, MPOk((), Customer))
    }
  }
}
case class MPOk(Agency: Unit, Customer: Out[binary.Ok]) {
  def send(v: Ok): Unit = {
    val cnt: Unit = Customer ! binary.Ok(v.p)
    ()
  }
}
