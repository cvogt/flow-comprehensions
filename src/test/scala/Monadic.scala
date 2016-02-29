package org.cvogt.test.flow
import scala.language.higherKinds
import org.scalatest._
import org.cvogt.flow._

import scalaz.concurrent.Task

class MonadicTests extends FunSuite with org.scalactic.TypeCheckedTripleEquals {

  case class Person(name: String, livesAt: Int, isRich: Boolean )
  case class Address(city: String)

  object Data {
    val people = Map(
      1 -> Person("Peter", 1, false),
      2 -> Person("Kevin", 2, true)
    )
    val addresses = Map(
      1 -> Address("Brooklyn"),
      2 -> Address("Upper East Side")
    )
  }

  object Dao {
    def getPerson(id: Int): Task[Person] =
      Task.delay(Data.people(id))
    def getAddress(id: Int): Task[Address] =
      Task.delay(Data.addresses(id))
  }

  test("support something like the vision") {
    // val description = flat[Task] {
    //   val person = Dao.getPerson(1).value
    //   val address = Dao.getAddress(person.livesAt).value
    //   val name = person.name
    //   val rich = if (person.isRich) "is rich" else "is not rich"
    //   s"$name $rich and lives at $address"
    // }
    // assert(description.unsafePerformSync == "Peter is not rich and lives at Brooklyn")
  }
}
