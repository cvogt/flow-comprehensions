package org.cvogt.test.flow
import scala.language.higherKinds
import scala.concurrent._
import scala.concurrent.duration._
import org.scalatest._
import org.cvogt.flow._

class Tests extends FunSuite with org.scalactic.TypeCheckedTripleEquals {
  implicit object SynchronousExecutionContext extends ExecutionContext {
    def execute( runnable: Runnable ): Unit = runnable.run
    def reportFailure( cause: Throwable ): Unit = throw new Exception( cause )
  }
  implicit class SynchronousFutureExtensions[T]( f: Future[T] ) {
    /** for testing purposes */
    def get( implicit sec: SynchronousExecutionContext.type ) = Await.result( f, 0.seconds )
  }

  test("flow comprehensions tests"){
    assert(
      Option(5) === flat[Option]{
        c => 5
      }
    )
    assert(
      Set(5) == flat[Set]{ c =>
        val o = c?Set(5)
        o
      }
    )
    assert(
      Option(15) == flat[Option]{ c =>
        val o1 = c?Option(5)
        val o2 = c?Option(3)
        o1 * o2
      }
    )

    assert(
      Option(15) == flat[Option]{ c =>
        val o1 = c?Option(5)
        val o2 = c?Option(3)
        o1 * o2
      }
    )

    assert(
      Option(30) == flat[Option]{ c =>
        val o1 = c?Option(5)
        val x = 2
        val o2 = c?Option(3)
        x * o1 * o2
      }
    )

    assert(
      List(3,4,6,8) == flat[List]{ c =>
        val l1 = c?List(1,2)
        val l2 = c?List(3,4)
        l1 * l2
      }
    )

    assert(
      List(15,18,20,24,30,36,40,48) == flat[List]{ c =>
        val l1 = c?List(1,2)
        val l2 = c?List(3,4)
        val l3 = c?List(5,6)
        l3 * l1 * l2
      }
    )

    assert(
      Seq(4,3,8,6) == flat[Seq] { c =>
        c?Seq(1,2) * c?Seq(4,3)
      }
    )

    assert(
      Seq(4,3,8,6) == flat[Seq] { c =>
        c?Seq(1,2) * c?Seq(4,3)
      }
    )

    assert(
      Seq(4,3,8,6) == flat[Seq] {
        Seq(1,2).value * Seq(4,3).value
      }
    )

    assert(
      None == flat[Option] {
        Option(3).value + Option.empty[Int].value
      }
    )

    assert(
      List("no", "no", "yes", "no", "yes", "yes") == flat[List] {
        if (List(1,2,3).value > List(1,2).value) "yes"
        else "no"
      }
    )

    assert(
      List(5, 6, 6, 7, 7, 8) == flat[List] {
        if (List(1).value > List(0).value) List(1,2,3).value + List(4,5).value
        else List.empty[Int].value
      }
    )

  }
}
