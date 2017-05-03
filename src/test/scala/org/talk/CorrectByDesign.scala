package org.talk

import org.scalatest.{FunSpec, Matchers}

class CorrectByDesign extends FunSpec with Matchers {
  /**
    * Pitch : how to model your data to avoid invalid state
    */

  describe("model a kebab") {
    it("is done the usual way") {
      case class Ingredient(name: String, isVegan: Boolean)
      case class Kebab(ingredients: List[Ingredient]) {
        def isVegan: Boolean = ingredients.forall(_.isVegan)
      }

      /**
        * Problems :
        *   - can create invalid ingredients => Ingredient("Meat", true)
        *   - hard to extends to other diets => Ingredient(name: String, isVegan: Boolean, isGluentFree: Boolean, ...)
        */
    }

    it("avoids invalid state") {
      sealed trait Diet
      case object Vegan extends Diet

      sealed class Ingredient(val compatibleDiets: List[Diet])
      case object Meat extends Ingredient(List(Vegan))

      case class Kebab(ingredients: List[Ingredient]) {
        def is(diet: Diet): Boolean = ingredients.forall(_.compatibleDiets.contains(diet))
        def isVegan: Boolean = is(Vegan)
      }
    }
  }
}
