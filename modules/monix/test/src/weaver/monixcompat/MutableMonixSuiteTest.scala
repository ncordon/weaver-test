package weaver
package monixcompat

import weaver.framework.DogFood
import weaver.monixcompat.modules._

import cats.effect.Resource
import monix.eval.Task
import sbt.testing.Status
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import monix.execution.atomic.Atomic


object MutableMonixSuiteTest extends SimpleMonixSuite {

  pureTest("23 is odd") {
    expect(23 % 2 == 1)
  }

  simpleTest("sleeping") {
    for {
      before <- timer.clock.realTime(TimeUnit.MILLISECONDS)
      _      <- timer.sleep(1.seconds)
      after  <- timer.clock.realTime(TimeUnit.MILLISECONDS)
    } yield expect(after - before >= 1000)
  }

  test("23 is odd") {
    expect(23 % 2 == 1)
  }

  loggedTest("logged") { log =>
    log.info("hello").map(_ => success)
  }
}

object MonixSuiteTest extends MonixSuite {
  type Res = KVStore

  def sharedResource = Resource.pure {
    val map = Atomic(Map.empty[String, String])
    new KVStore.RefBased(map)
  }

  override def maxParallelism: Int = 1

  // Don't do this at home, kids
  test("setting some value in a shared store") { (kvStore, log) =>
    for {
      _          <- timer.sleep(1.seconds)
      _          <- kvStore.put("hello", "world")
      helloValue <- kvStore.get("hello")
      _          <- log.info(helloValue.getOrElse("empty"))
    } yield expect(List(1, 2, 3).size == 3)
  }

  test("getting the value set in a previous test") { kvStore =>
    for {
      previous <- kvStore.delete("hello")
      now      <- kvStore.get("hello")
    } yield expect(previous == Some("world")) and expect(now == None)
  }

  List(
    TestWithExceptionInTest,
    TestWithExceptionInExpectation,
    TestWithExceptionInInitialisation,
    TestWithEventualDiedSharedLayer,
    TestWithFailedSharedLayer
  ).foreach { testSuite =>
    test(s"fail properly in ${testSuite.getClass.getSimpleName}") {
      for {
        (_, events) <- DogFood.runSuite(testSuite).to[Task]
      } yield {
        val event = events.headOption.get
        expect(event.status() == Status.Error) and
          expect(event.throwable().get().getMessage == "oh no")
      }
    }
  }

  test("fail properly on failed expectations") {
    for {
      (_, events) <- DogFood.runSuite(TestWithFailedExpectation).to[Task]
    } yield expect(events.headOption.get.status() == Status.Failure).toExpectations
  }

  object TestWithExceptionInTest extends SimpleMonixSuite {
    test("example test") {
      Task.raiseError(new RuntimeException("oh no"))
    }
  }

  object TestWithExceptionInExpectation extends SimpleMonixSuite {
    test("example test") {
      for {
        _ <- Task.unit
      } yield throw new RuntimeException("oh no")
    }
  }

  object TestWithExceptionInInitialisation extends SimpleMonixSuite {
    test("example test") { _ =>
      throw new RuntimeException("oh no")
    }
  }

  object TestWithFailedExpectation extends SimpleMonixSuite {
    test("example test") { _ =>
      for {
        _ <- Task.unit
      } yield expect(false)
    }
  }

  object TestWithFailedSharedLayer extends MutableMonixSuite {
    override val sharedResource = 
      Resource.liftF(Task.raiseError(new RuntimeException("oh no")))

    test("example test") {
      expect(true)
    }
  }

  object TestWithEventualDiedSharedLayer extends MutableMonixSuite {
    type Res = Unit

    override val sharedResource =
      Resource.liftF(Task.delay(throw new RuntimeException("oh no")))

    test("example test") {
      expect(true)
    }
  }
}

object modules {
  type KVStore = KVStore.Service

  object KVStore {
    trait Service {
      def put(k: String, v: String): Task[Unit]
      def get(k: String): Task[Option[String]]
      def delete(k: String): Task[Option[String]]
    }

    class RefBased(ref: Atomic[Map[String, String]]) extends Service {
      def put(k: String, v: String): Task[Unit] =
        for {
          m <- Task.delay(ref.get())
          _ <- Task.delay(ref.update(m + (k -> v)))
        } yield ()

      def get(k: String): Task[Option[String]] = 
        Task.delay(ref.get().get(k))

      def delete(k: String): Task[Option[String]] =
        for {
          m <- Task.delay(ref.get())
          res = m.get(k)
          _ <- Task.delay(ref.update(m - k))
        } yield res
    }
  }
}