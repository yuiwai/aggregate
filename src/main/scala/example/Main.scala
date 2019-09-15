package example

import aggregate.Repository.{Locked, NotFound}
import aggregate.{Created, Executed, Existed, Modified, Operation, Put, Reset}

import scala.util.chaining._

object Main {
  val car1 = Car(new Motor, Map.empty, None)
  val motor1 = new Motor
  val motor2 = new Motor
  val tire1 = new Tire
  val navi1 = new Navigation
  val navi2 = new Navigation

  def main(args: Array[String]): Unit = {
    factory()
    aggregate()
    operation()
    repository()
  }

  def factory(): Unit = {
    CarAgg.makeWithHistory(100)(car1).tap {
      case Executed(agg, histories) =>
        assert(agg.root == car1)
        assert(histories == Seq(Created(100, car1)))
    }
  }

  def aggregate(): Unit = {
    val car = CarAgg(100, Car(motor1, Map.empty, Some(navi1)))
    car.execute(ChangeMotor(motor2)).tap { case Executed(agg, histories) =>
      assert(agg.root.motor == motor2)
      assert(histories == Seq(Reset(motor2)))
    }
    car.execute(PutTire(FL, tire1)).tap { case Executed(agg, histories) =>
      assert(agg.root.tires(FL) == tire1)
      assert(histories == Seq(Put(FL, tire1)))
    }
    car.execute(PutTireAll(tire1)).tap { case Executed(agg, histories) =>
      assert(agg.root.tires.size == 4)
      assert(agg.root.tires.values.forall(_ == tire1))
      assert(histories.size == 4)
      assert(histories.forall(_.asInstanceOf[Put[Pos, Tire]].entity == tire1))
    }

    car.execute(ExistNavigation(navi2)).tap { a => assert(a.agg.root.navigation.contains(navi2)) }
    car.execute(EmptyNavigation).tap { a => assert(a.agg.root.navigation.isEmpty) }
  }

  def operation(): Unit = {
    Operation(CarAgg.makeWithHistory(100)(car1), Seq.empty)
      .tap { op =>
        op.execute
          .tap { r =>
            assert(r.agg.root == car1)
          }
        op.add(ExistNavigation(navi1))
          .execute
          .tap { r =>
            assert(r.agg.root == car1.copy(navigation = Some(navi1)))
            assert(r.histories == Seq(Existed(navi1)))
          }
        op.add(ExistNavigation(navi2)).add(PutTireAll(tire1)).add(ChangeMotor(motor2))
          .execute
          .tap { r =>
            assert(r.agg.root == car1.copy(motor2, Pos.all.map(_ -> tire1).toMap, Some(navi2)))
            assert(r.histories == Seq(Existed(navi2), Put(FL, tire1), Put(FR, tire1), Put(RL, tire1), Put(RR, tire1), Reset(motor2)))
          }
      }
  }

  def repository(): Unit = {
    val repo = new InMemoryCarRepository

    repo.lock(100)
      .tap(r => assert(r == Left(NotFound)))
    repo.store(100, CarAgg.makeWithHistory(100)(car1).asResult)
      .tap(r => assert(r == 100))
    repo.resolve(100)
      .tap(r => assert(r.contains(car1)))
    repo.lock(100)
      .tap(r => assert(r == Right(car1)))
    repo.lock(100)
      .tap(r => assert(r == Left(Locked)))
    repo.unlock(100).lock(100)
      .tap(r => assert(r == Right(car1)))
  }
}

