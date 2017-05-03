package org.talk

import org.scalatest.{FunSpec, Matchers}

class RefactorToFunctionnal extends FunSpec with Matchers {
  /**
    * Spec :
    *   - calculate average salary for employees
    *   - able to exclude lowest salaries
    *   - able to filtrate by department
    * source https://vsavkin.com/functional-typescript-316f0e003dc6
    */


  case class Employee(name: String, salary: Double)
  case class Department(employees: List[Employee]) {
    def works(employee: Employee): Boolean = employees.contains(employee)
  }

  val employees = List(
    Employee("Jim", 100),
    Employee("John", 200),
    Employee("Liz", 120),
    Employee("Penny", 30)
  )
  val sales = Department(employees.take(2))


  it("shows some imperative code") {
    def averageSalary(employees: List[Employee], minSalary: Double, department: Department): Double = {
      var total = 0d
      var count = 0d

      for(e <- employees) {
        if(e.salary >= minSalary && (department == null || department.works(e))){
          total += e.salary
          count += 1
        }
      }

      if(count == 0) 0 else total / count
    }

    averageSalary(employees, 50, sales) shouldBe 150
    averageSalary(employees, 50, null) shouldBe 140

    /**
      * hard to extend, add a new condition means :
      *   - add a new argument => change public interface
      *   - change implementation
      *     - logic will grow forever will become unreadable (eventually)
      *     - any change at this function will (potentially) impact a lot of code
      *     - need all possible values in arguments even if not used...
      *     - maybe at some point someone will write the same logic but with other conditions => duplication
      * other problems :
      *   - don't know which parameters are optional (department is optional but not minSalary)
      *   - behaviour on empty list is not clear
      */
  }


  it("replaces values by functions") {
    type Predicate = Employee => Boolean

    def averageSalary(employees: List[Employee], salaryCondition: Predicate, departmentCondition: Predicate): Double = {
      var total = 0d
      var count = 0d

      for(e <- employees) {
        if(salaryCondition(e) && (departmentCondition == null || departmentCondition(e))){
          total += e.salary
          count += 1
        }
      }

      if(count == 0) 0 else total / count
    }

    averageSalary(employees, e => e.salary >= 50, e => sales.works(e)) shouldBe 150
    averageSalary(employees, _.salary >= 50, sales.works) shouldBe 150
    averageSalary(employees, _.salary >= 50, null) shouldBe 140

    /**
      * now all parameters are the same (except first one) which will allow to generalize...
      */
  }


  it("group predicates in a list") {
    type Predicate = Employee => Boolean

    def averageSalary(employees: List[Employee], predicates: List[Predicate]): Double = {
      var total = 0d
      var count = 0d

      for(e <- employees) {
        if(predicates.forall(predicate => predicate(e))){
          total += e.salary
          count += 1
        }
      }

      if(count == 0) 0 else total / count
    }

    def salaryGreaterThan(value: Double): Predicate = (e: Employee) => e.salary >= value
    def belongTo(department: Department): Predicate = (e: Employee) => department.works(e)
    averageSalary(employees, List(salaryGreaterThan(50), belongTo(sales))) shouldBe 150
    averageSalary(employees, List(salaryGreaterThan(50))) shouldBe 140

    /**
      * now it's simpler to extend, add a new condition means :
      *   - no new argument => function interface is more robust
      *   - no code change => no incontrolled impact or code growing infinitely
      *   - only used predicates (no more optional parameters)
      *   - only add an other item to the list in parameter
      *
      * new we can calculate average salaries for any condition we want !!!
      */
  }


  it("one predicate to rule them all") {
    type Predicate = Employee => Boolean

    def and(predicates: Predicate*): Predicate =
      (e: Employee) => predicates.forall(predicate => predicate(e))

    def averageSalary(employees: List[Employee], predicate: Predicate): Double = {
      var total = 0d
      var count = 0d

      for(e <- employees) {
        if(predicate(e)){
          total += e.salary
          count += 1
        }
      }

      if(count == 0) 0 else total / count
    }

    def salaryGreaterThan(value: Double): Predicate = (e: Employee) => e.salary >= value
    def belongTo(department: Department): Predicate = (e: Employee) => department.works(e)
    averageSalary(employees, and(salaryGreaterThan(50), belongTo(sales))) shouldBe 150
    averageSalary(employees, and(salaryGreaterThan(50))) shouldBe 140

    def or[T](predicates: (T => Boolean)*): T => Boolean = e => predicates.exists(predicate => predicate(e))
    def not[T](predicate: T => Boolean): T => Boolean = e => !predicate(e)

    def salaryIn(min: Double, max: Double): Predicate = and(salaryGreaterThan(min), not(salaryGreaterThan(max)))
    averageSalary(employees, or(salaryIn(50, 250), belongTo(sales))) shouldBe 140

    /**
      * predicates are composables !!!
      */
  }


  it("uses pipelines to transform data") {
    type Predicate = Employee => Boolean

    // filter employees before iterating
    def averageSalary1(employees: List[Employee], predicate: Predicate): Double = {
      val filtered = employees.filter(predicate)

      var total = 0d
      var count = 0d

      for(e <- filtered) {
        total += e.salary
        count += 1
      }

      if(count == 0) 0 else total / count
    }

    // count is not useful anymore
    def averageSalary2(employees: List[Employee], predicate: Predicate): Double = {
      val filtered = employees.filter(predicate)

      var total = 0d

      for(e <- filtered) {
        total += e.salary
      }

      if(filtered.length == 0) 0 else total / filtered.length
    }

    // if we map salaries, total is just a fold
    def averageSalary3(employees: List[Employee], predicate: Predicate): Double = {
      val filtered = employees.filter(predicate)
      val total = filtered.map(_.salary).foldLeft(0d)((total, salary) => total + salary)

      if(filtered.length == 0) 0 else total / filtered.length
    }

    // extract average
    def average(nums: List[Double]): Double =
      if(nums.length == 0) 0 else nums.foldLeft(0d)((sum, cur) => sum + cur) / nums.length

    def averageSalary4(employees: List[Employee], predicate: Predicate): Double =
      average(employees.filter(predicate).map(_.salary))

    // more Scalaish
    def averageScala(nums: List[Double]): Double =
      if(nums.isEmpty) 0 else nums.sum / nums.length

    def and(predicates: Predicate*): Predicate = (e: Employee) => predicates.forall(predicate => predicate(e))
    def salaryGreaterThan(value: Double): Predicate = (e: Employee) => e.salary >= value
    def belongTo(department: Department): Predicate = (e: Employee) => department.works(e)
    averageSalary1(employees, and(salaryGreaterThan(50), belongTo(sales))) shouldBe 150
    averageSalary2(employees, and(salaryGreaterThan(50), belongTo(sales))) shouldBe 150
    averageSalary3(employees, and(salaryGreaterThan(50), belongTo(sales))) shouldBe 150
    averageSalary4(employees, and(salaryGreaterThan(50), belongTo(sales))) shouldBe 150

    /**
      * Split generic and business code
      *
      * Benefits :
      *   - more generic / robust
      *     - easy to extend
      *     - hard to introduce bugs
      *   - no mutable state
      *   - easy to test
      *   - easier to understand
      *   - dead simple functions !
      */
  }


  it("adds genericity") {
    type Predicate[T] = T => Boolean
    def and[T](predicates: Predicate[T]*) = (t: T) => predicates.forall(predicate => predicate(t))
    def average(nums: List[Double]): Double = if(nums.isEmpty) 0 else nums.sum / nums.length

    def averageSalary(employees: List[Employee], predicate: Predicate[Employee]): Double =
      average(employees.filter(predicate).map(_.salary))

    def salaryGreaterThan(value: Double): Predicate[Employee] = (e: Employee) => e.salary >= value
    def belongTo(department: Department): Predicate[Employee] = (e: Employee) => department.works(e)
    averageSalary(employees, and(salaryGreaterThan(50), belongTo(sales))) shouldBe 150

    /**
      * Generic code is really generic \o/
      */
  }


  it("split parameters to allow currying") {
    type Predicate[T] = T => Boolean
    def and[T](predicates: Predicate[T]*) = (t: T) => predicates.forall(predicate => predicate(t))
    def average(nums: List[Double]): Double = if(nums.isEmpty) 0 else nums.sum / nums.length

    def averageSalary(predicate: Predicate[Employee])(employees: List[Employee]): Double =
      average(employees.filter(predicate).map(_.salary))

    def salaryGreaterThan(value: Double): Predicate[Employee] = (e: Employee) => e.salary >= value
    def belongTo(department: Department): Predicate[Employee] = (e: Employee) => department.works(e)
    averageSalary(and(salaryGreaterThan(50), belongTo(sales)))(employees) shouldBe 150

    def averageSalesSalary = averageSalary(and(salaryGreaterThan(50), belongTo(sales))) _
    averageSalesSalary(employees) shouldBe 150
  }


  it("compares approches") {
    def averageSalaryImp(employees: List[Employee], minSalary: Double, department: Department): Double = {
      var total = 0d
      var count = 0d

      for(e <- employees) {
        if(minSalary <= e.salary && (department == null || department.works(e))){
          total += e.salary
          count += 1
        }
      }

      if(total == 0) 0 else total / count
    }

    averageSalaryImp(employees, 50, sales) shouldBe 150

    // VS

    // generic code
    type Predicate[T] = T => Boolean
    def and[T](predicates: Predicate[T]*) = (t: T) => predicates.forall(predicate => predicate(t))
    def average(nums: List[Double]): Double = if(nums.isEmpty) 0 else nums.sum / nums.length

    // business function
    def averageSalaryFunc(predicate: Predicate[Employee])(employees: List[Employee]): Double =
      average(employees.filter(predicate).map(_.salary))

    averageSalaryFunc(and[Employee](_.salary >= 50, sales.works))(employees) shouldBe 150

    // clear predicates
    def salaryGreaterThan(value: Double): Predicate[Employee] = (e: Employee) => e.salary >= value
    def belongTo(department: Department): Predicate[Employee] = (e: Employee) => department.works(e)
    averageSalaryFunc(and(salaryGreaterThan(50), belongTo(sales)))(employees) shouldBe 150
  }


  it("way too generic/abstract") {
    type Predicate[T] = T => Boolean
    def and[T](predicates: Predicate[T]*) = (t: T) => predicates.forall(predicate => predicate(t))
    def average(nums: List[Double]): Double = if(nums.isEmpty) 0 else nums.sum / nums.length

    def averageAnything[T](predicate: Predicate[T])(transform: T => Double)(items: List[T]): Double =
      average(items.filter(predicate).map(transform))

    averageAnything[Employee](and[Employee](_.salary >= 50, sales.works))(_.salary)(employees) shouldBe 150
    averageAnything[Int](_ => true)(_.toDouble)(List(1, 2, 3)) shouldBe 2

    /**
      * Know when to stop ;)
      */
  }
}
