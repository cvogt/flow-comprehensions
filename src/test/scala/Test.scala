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
      Option(5) === sequence[Option]{
        c => 5
      }
    )
    assert(
      Option(5) == sequence[Option]{ c =>
        val o = ~Option(5)
        o
      }
    )
    assert(
      Option(15) == sequence[Option]{ c =>
        val o1 = ~Option(5)
        val o2 = ~Option(3)
        o1 * o2
      }
    )
    /* // crashes scalac
    assert(
      Option(5) == sequence[Option]{ c =>
        val o = Option(5)
        o
      }
    )
    */

    /*
    // cartesian, inside assignment
    assert(
      sequence[List]{
        val i = ~List(1,2)
        val j = ~List(2,3)
        (i,j)
      }    
      ===
      ( for{
        i <- List(1,2)
        j <- List(2,3)
      } yield (i,j) )
    )
    assert(
      sequence[Future]{
        val i = ~Future(1)
        val j = ~Future(2)
        (i,j)
      }    .get
      ===
      ( for{
        i <- Future(1)
        j <- Future(2)
      } yield (i,j) ).get
    )

    // self cartesian, inline, outside-reference
    {    
      val m = List(1)
      assert(
        sequence[List]{ (~m,~m) }
        === 
        ( for{ i <- m; j <- m } yield (i,j) )
      )
    }

    {    
      val m = Future(1)
      assert(
        sequence[Future]{ (~m,~m) }.get
        === 
        ( for{ i <- m; j <- m } yield (i,j) ).get
      )
    }

    // double reference
    assert(
      sequence[List]{ val i = ~List(1); (i,i) }
      === 
      ( for{ i <- List(1) } yield (i,i) )
    )
    assert(
      sequence[Future]{ val i = ~Future(1); (i,i) }.get
      === 
      ( for{ i <- Future(1) } yield (i,i) ).get
    )

    // comprehension transformers
    val m1 = List(0,1,2)
    assert(
      sequence[List]{ c =>
        val i = ~m1
        val j = ~List(2,3)
        c.filter(i > 0).reverse take 2 // TODO: maybe require an explicit call (M[FlowContext] => Unit), e.g. `.!`
        i * j
      }    
      ===
      ( for{
        i <- m1
        j <- List(2,3)
        if i > 0
      } yield i * j ).reverse.take(2) 
    )

    {
      import implicits.autoEmbed    
      assert(
        sequence[List]{ List(1,2) * List(2,3) }
        ===
        (for{
          i <- List(1,2)
          j <- List(2,3)
        } yield i * j)
      )
      assert(
        sequence[List]{
          List(1,2) * sequence[List]{ List(2,3) * List(4,5) }
        }
        ===
        (for{
          i <- List(1,2)
          j <- (
            for{
              i <- List(1,2)
              j <- List(2,3)
            } yield i * j
          )
        } yield i * j)
      )
    }
*/
  }
}
