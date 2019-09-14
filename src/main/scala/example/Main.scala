package example

import aggregate.{Executed, Put, Reset}

import scala.util.chaining._

object Main {
  def main(args: Array[String]): Unit = {
    basic()
  }

  def basic(): Unit = {
    val motor1 = new Motor
    val motor2 = new Motor
    val tire1 = new Tire
    val navi1 = new Navigation
    val navi2 = new Navigation

    assert(motor1 != motor2)

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
}

