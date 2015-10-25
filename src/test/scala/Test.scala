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

    assert(
      Option(15) == sequence[Option]{ c =>
        val o1 = ~Option(5)
        val o2 = ~Option(3)
        o1 * o2
      }
    )

    assert(
      Option(30) == sequence[Option]{ c =>
        val o1 = ~Option(5)
        val x = 2
        val o2 = ~Option(3)
        x * o1 * o2
      }
    )

    assert(
      List(3,4,6,8) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        l1 * l2
      }
    )

    assert(
      List(8,6,4,3) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        c(_.reverse)
        l1 * l2
      }
    )

    assert(
      List(3,4,6,8) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        c(_.reverse)
        c(_.reverse)
        l1 * l2
      }
    )

    assert(
      List(32,24,8,6) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        val unrelated = 2
        def foo(i: Int) = i
        val unrelated2 = foo(l1)
        c(_.reverse)
        // foo(l1) // does not compile
        l1 * l2 * unrelated * unrelated2
      }
    )

    assert(
      List(15,18,20,24,30,36,40,48) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        val l3 = ~List(5,6)
        l3 * l1 * l2
      }
    )

    assert(
      List(48,40,36,30,24,20,18,15) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        val l3 = ~List(5,6)
        c(_.reverse)
        l3 * l1 * l2
      }
    )

    assert(
      List(40,48,30,36,20,24,15,18) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        c(_.reverse)
        val l3 = ~List(5,6)
        l3 * l1 * l2
      }
    )

    def reverse[T] = (_:List[T]).reverse

    assert(
      List(40,48,30,36,20,24,15,18) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        c(reverse)
        val l3 = ~List(5,6)
        l3 * l1 * l2
      }
    )

    implicit class IntListExtensions[T](l: List[T]){
      def reverse2 = l.reverse
    }
    assert(
      List(40,48,30,36,20,24,15,18) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        c(_.reverse2)
        val l3 = ~List(5,6)
        l3 * l1 * l2
      }
    )

    /*
    assert(
      List(15,18,20,24,30,36,40,48) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        val l3 = ~List(5,6)
        c(_.map(identity))
        l3 * l1 * l2
      }
    )
    */

    assert(
      List(4,3,8,6) == sequence[List]{ c =>
        val l1 = ~List(2,1)
        c(_.sortBy(_ => l1))
        val l2 = ~List(4,3)
        l1 * l2
      }
    )

    assert(
      List(4,3,8,6) == sequence[List]{ c =>
        ~List(1,2) * ~List(4,3)
      }
    )

/*
    assert(
      List(6,8) == sequence[List]{ c =>
        val l1 = ~List(1,2)
        val l2 = ~List(3,4)
        c.filter(l1 >= 2)
        l1 * l2
      }
    )*/
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
