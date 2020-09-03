package weaver
package monixcompat


import cats.effect.{ConcurrentEffect, Resource}

import monix.eval.Task
import monix.eval.instances.CatsConcurrentEffectForTask


trait MutableMonixSuite extends MutableFSuite[Task] with BaseMonixSuite

trait SimpleMutableMonixSuite extends MutableMonixSuite{
  type Res = Unit
  def sharedResource: Resource[Task, Unit] = Resource.pure(())
}

trait BaseMonixSuite { self : ConcurrentEffectSuite[Task] =>
  implicit def executor = monix.execution.Scheduler.Implicits.global
  implicit def opts = Task.defaultOptions
  implicit def timer = Task.timer
  implicit def effect : ConcurrentEffect[Task] = new CatsConcurrentEffectForTask()
}

trait PureMonixSuite extends ConcurrentEffectSuite[Task] with BaseMonixSuite {

  def pureTest(name: String)(run : => Expectations) : Task[TestOutcome] = Test[Task](name, Task(run))
  def simpleTest(name:  String)(run : Task[Expectations]) : Task[TestOutcome] = Test[Task](name, run)
  def loggedTest(name: String)(run : Log[Task] => Task[Expectations]) : Task[TestOutcome] = Test[Task](name, run)
}
