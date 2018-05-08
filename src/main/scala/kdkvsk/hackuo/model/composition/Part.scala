package kdkvsk.hackuo.model.composition

import cats.arrow.FunctionK
import cats.data.State
import kdkvsk.hackuo._

trait Part[T <: Part[T]]

trait PartOps[W, P] {
  def get(w: W): P
  def set(w: W, t: P): W

  def handler: PartialFunction[Message, State[P, Response]]

  def worldHandler: PartialFunction[Message, State[W, Response]] = handler.andThen { p =>
    State.apply[W, Response] { w =>
      val (newP, r) = p.run(get(w)).value
      (set(w, newP), r)
    }
  }

  implicit def unit2response(s: State[P, Unit]): State[P, Response] = {
    s.map(_ => NoopResponse)
  }

  implicit def event2response(s: State[P, Event]): State[P, Response] = {
    s.map(e => EventResponse(e))
  }

  implicit def eventOpt2response(s: State[P, Option[Event]]): State[P, Response] = {
    s.map {
      case Some(e) => EventResponse(e)
      case None => NoopResponse
    }
  }

  implicit def eventSeq2response(s: State[P, Seq[Event]]): State[P, Response] = {
    s.map(es => es.foldLeft(NoopResponse: Response) {
      case (r, e) => r.combine(EventResponse(e))
    })
  }
}

object PartOps {
  def combine[P](one: PartialFunction[Message, State[P, Response]], fs: (PartialFunction[Message, State[P, Response]])*): PartialFunction[Message, State[P, Response]] = {
    fs.foldLeft(one)(combineTwo)
  }


  def combineTwo[P](one: PartialFunction[Message, State[P, Response]], two: PartialFunction[Message, State[P, Response]]): PartialFunction[Message, State[P, Response]] = new PartialFunction[Message, State[P, Response]] {
    def isDefinedAt(m: Message): Boolean = {
      one.isDefinedAt(m) || two.isDefinedAt(m)
    }

    def apply(m: Message): State[P, Response] = {
      val oneDefined: Boolean = one.isDefinedAt(m)
      val twoDefined: Boolean = two.isDefinedAt(m)

      if (oneDefined && twoDefined) {
        stateCombine(one(m), two(m))
      } else if (oneDefined) {
        one(m)
      } else if (twoDefined) {
        two(m)
      } else {
        State.pure(NoopResponse)
      }
    }
  }

  def stateCombine[P](one: State[P, Response], two: State[P, Response]): State[P, Response] = {
    for {
      r1 <- one
      r2 <- two
    } yield {
      r1.combine(r2)
    }
  }
}