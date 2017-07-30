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

  describe("useful types") {
    it("basic types does not give any insight") {
      trait Constraint
      case class Link(from: String, action: String, to: String, constraints: List[Constraint])

      // what does this function do ?
      def verify(workflow: List[Link], from: String, action: String, data: Any): List[String] = ???
    }

    it("is necessary to look at the implementation") {
      trait Constraint {
        def verify(data: Any): List[String]
      }
      case class Link(from: String, action: String, to: String, constraints: List[Constraint])

      // and this one ?
      def verify(workflow: List[Link], from: String, action: String, data: Any): List[String] =
        workflow.find(l => l.from == from && l.action == action)
          .map(_.constraints.flatMap(_.verify(data)).toList)
          .getOrElse(List(s"action $action not available for state $from"))
    }

    it("specifics types tells a lot more") {
      type State = String
      type Action = String
      trait Constraint
      case class Link(from: String, action: String, to: String, constraints: List[Constraint])
      type Workflow = List[Link]
      type Error = String

      // what about this ?
      def verify(workflow: Workflow, from: State, action: Action, data: Any): List[Error] = ???
    }

    it("improve typing & safety") {
      sealed abstract class WorkflowError(text: String)
      case class NotFound(from: State, action: Action) extends WorkflowError(s"action $action not available for state $from")
      sealed trait State
      sealed trait Action
      sealed trait Constraint {
        def verify(data: Any): List[WorkflowError]
      }
      case class Link(from: State, action: Action, to: State, constraints: List[Constraint])
      case class Workflow(links: List[Link]) {
        def get(from: State, action: Action): Option[Link] = links.find(l => l.from == from && l.action == action)
      }

      def verify(workflow: Workflow, from: State, action: Action, data: Any): List[WorkflowError] =
        workflow.get(from, action)
          .map(_.constraints.flatMap(_.verify(data)).toList)
          .getOrElse(List(NotFound(from, action)))
    }

    // now imagine JavaScript: function verify(workflow, from, action, data){}
  }
}
