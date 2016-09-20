package edu.colorado.plv.aiwala

import org.scalatest._

class ConcreteInterpreterSpec extends FlatSpec {
  import ConcreteInterpreter._

  "sum" should "sum the elements of a given list" in {
    assertResult(6) {
      sum(List(1,2,3))
    }
  }

  it should "return 0 for an empty list" in {
    assertResult(0) {
      sum(Nil)
    }
  }

}
