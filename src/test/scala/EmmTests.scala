package org.cvogt.test.flow
import scala.language.higherKinds
import scala.concurrent._
import scala.concurrent.duration._
import org.scalatest._
import org.cvogt.flow._

import emm._
import emm.compat.scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.std.option._

class EmmTests extends FunSuite with org.scalactic.TypeCheckedTripleEquals {
  test("monad stack test") {
    def readName: Task[String] = Monad[Task].point("chris")
    def log(msg: String): Task[Unit] = Monad[Task].point(())

    type E = Task |: (String \/ ?) |: Option |: Base

    type MyEmm[T] = Emm[E, T]
    implicit def MyEmmFactory = new Construct[MyEmm]{
      def create[T](v: T) = Emm.point(v)
    }

    //implicit containsTyped[E,T <: ]()

    import scala.language.implicitConversions
    implicit def liftTask[T](t: Task[T]) = t.liftM[E]
    implicit def liftOption[T](t: Option[T]) = t.liftM[E]
    implicit def liftEither[T](t: String \/ T) = t.liftM[E]

    val effect: MyEmm[String] = flat[MyEmm] { c =>
      val first = c?(readName.liftM[E])
      val last = c?(readName.liftM[E])

      val name = c?(
        if ((first.length + last.length) < 20) Some(s"$first $last")
        else (Option.empty[String])
      ).liftM[E]

      c?(
        (
          if (name == "Daniel Spiewak") -\/("your kind isn't welcome here")
          else \/-(())
        ).liftM[E]
      )

      c?(log(s"successfully read in $name").liftM[E])

      name
    }
    assert(effect.run.unsafePerformSync == \/-(Some("chris chris")))

  }
}
