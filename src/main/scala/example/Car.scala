package example

import aggregate.Repository.{LockError, Locked, NotFound}
import aggregate._
import example.CarRepository.Id

import scala.collection.mutable
import scala.util.chaining._

case class Car(motor: Motor, tires: Map[Pos, Tire], navigation: Option[Navigation])

class CarAgg private(val id: Int, val root: Car) extends SimpleAgg[Car] {
  override type Identifier = Int
  override type This = CarAgg
  override type Cmd = CarCmd
  implicit val motor: Ent[Car, Motor] = ent[Motor](_.motor)(root => f => root.copy(motor = f(root.motor)))
  implicit val tires: EntM[Car, Pos, Tire] = entM[Pos, Tire](_.tires)(r => m => r.copy(tires = m))
  implicit val navigation: EntO[Car, Navigation] = entO[Navigation](_.navigation)(r => m => r.copy(navigation = m(r.navigation)))

  override protected val modRoot: Car => CarAgg = new CarAgg(id, _)
  override def execute(command: CarCmd): Executed[Car] = command match {
    case ChangeMotor(newMotor) => changeMotor(newMotor)
    case PutTire(pos, tire) => putTire(pos, tire)
    case PutTireAll(tire) => putTireAll(tire)
    case ExistNavigation(newNavigation) => existNavigation(newNavigation)
    case EmptyNavigation => emptyNavigation()
  }
  private def changeMotor(newMotor: Motor): Executed[Car] = setWithHistory(newMotor)
  private def putTire(pos: Pos, newTire: Tire): Executed[Car] = putWithHistory(pos, newTire)
  private def putTireAll(tire: Tire): Executed[Car] =
    Pos.all.foldLeft(Executed(this, Seq.empty)) {
      case (Executed(agg, histories), pos) =>
        agg.asInstanceOf[CarAgg].putTire(pos, tire).pipe(ex => ex.copy(histories = histories ++ ex.histories))
    }
  private def existNavigation(newNavigation: Navigation) = existWithHistory(newNavigation)
  private def emptyNavigation() = emptyWithHistory[Navigation]
}
object CarAgg extends AggFactory[Int, Car] {
  override val make: Int => Car => Agg[Car] = id => root => apply(id, root)
  def apply(id: Int, car: Car): CarAgg = new CarAgg(id, car)
}

sealed trait CarCmd extends Command[Car]
final case class ChangeMotor(newMotor: Motor) extends CarCmd
final case class PutTire(pos: Pos, tire: Tire) extends CarCmd
final case class PutTireAll(tire: Tire) extends CarCmd
final case class ExistNavigation(newNavigation: Navigation) extends CarCmd
case object EmptyNavigation extends CarCmd

class Motor

sealed trait Pos
case object FL extends Pos
case object FR extends Pos
case object RL extends Pos
case object RR extends Pos
object Pos {
  val all: Seq[Pos] = Seq(FL, FR, RL, RR)
}

class Tire

class Navigation

sealed trait CarState
case object Stopped extends CarState
case object Running extends CarState

trait CarRepository extends Repository[Id, Int, Car]
object CarRepository {
  type Id[T] = T
}

class InMemoryCarRepository extends CarRepository {
  private val data = mutable.Map.empty[Int, Car]
  private val locked = mutable.Set.empty[Int]
  override def store(id: Int, result: Result[Car]): Int =
      id.tap(data.update(_, result.agg.root))
  override def resolve(id: Int): Option[Car] = data.get(id)
  override def lock(id: Int): Either[LockError, Car] =
      if (locked(id)) Left(Locked)
      else resolve(id).fold[Either[LockError, Car]](Left(NotFound))(Right(_).tap(_ => locked.add(id)))
  def unlock(id: Int): InMemoryCarRepository = this.tap(_ => locked.subtractOne(id))
}
