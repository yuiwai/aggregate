package aggregate

import aggregate.Repository.LockError

import scala.language.higherKinds
import scala.util.chaining._

trait Agg[R] {
  type Identifier
  val id: Identifier
  type This <: Agg[R]
  type Cmd <: Command[R]
  val root: R
  protected val modRoot: R => This
  def execute(command: Cmd): Executed[R]
  protected def mod[E](f: E => E)(implicit e: Ent[R, E]): This
  protected def set[E](v: E)(implicit e: Ent[R, E]): This
  protected def put[K, E](k: K, v: E)(implicit e: EntM[R, K, E]): This
  protected def ent[E](getter: R => E)(modifier: R => (E => E) => R): Ent[R, E]
  protected def entO[E](getter: R => Option[E])(modifier: R => (Option[E] => Option[E]) => R): EntO[R, E]
  protected def entM[K, E](mapperR: R => Map[K, E])(mapperW: R => Map[K, E] => R): EntM[R, K, E]
}
trait AggFactory[I, R] {
  val make: I => R => Agg[R]
  val makeWithHistory: I => R => Executed[R] = id => root => make(id)(root).pipe(a => Executed(a, Seq(Created(a.id, a.root))))
}

final case class Executed[R](agg: Agg[R], histories: Seq[History]) {
  def asResult: Result[R] = Result(agg, histories)
}
object Executed {
  def apply[R](agg: Agg[R]): Executed[R] = apply(agg, Seq.empty)
}

trait SimpleAgg[R] extends Agg[R] {
  protected def mod[E](f: E => E)(implicit e: Ent[R, E]): This = modRoot(e.mod(root)(f))
  protected def set[E](v: E)(implicit e: Ent[R, E]): This = modRoot(e.set(root)(v))
  // protected def empty[E](implicit e: EntO[R, E]): This = modRoot(e.empty(root))
  protected def put[K, E](k: K, v: E)(implicit e: EntM[R, K, E]): This = modRoot(e.put(root)(k)(v))
  protected def ent[E](getter: R => E)(modifier: R => (E => E) => R): Ent[R, E] = Ent[R, E](getter)(modifier)
  protected def entO[E](getter: R => Option[E])(modifier: R => (Option[E] => Option[E]) => R): EntO[R, E] = EntO(getter)(modifier)
  protected def entM[K, E](mapperR: R => Map[K, E])(mapperW: R => Map[K, E] => R): EntM[R, K, E] = EntM[R, K, E](mapperR)(mapperW)
  def modWithHistory[E](f: E => E)(implicit e: Ent[R, E]): Executed[R] =
    modRoot(e.mod(root)(f)).pipe { a =>
      Executed(a, Seq(Modified(e.get(a.root))))
    }
  def setWithHistory[E](v: E)(implicit e: Ent[R, E]): Executed[R] =
    modRoot(e.set(root)(v)).pipe { a => Executed(a, Seq(Reset(e.get(a.root)))) }
  def existWithHistory[E](v: E)(implicit e: EntO[R, E]): Executed[R] =
    modRoot(e.exist(root)(v)).pipe { a => Executed(a, Seq(Existed(v))) }
  def emptyWithHistory[E](implicit e: EntO[R, E]): Executed[R] =
    modRoot(e.empty(root)).pipe { a => Executed(a, Seq(Emptied[E]())) }
  def putWithHistory[K, E](k: K, v: E)(implicit e: EntM[R, K, E]): Executed[R] =
    modRoot(e.put(root)(k)(v)).pipe { a => Executed(a, Seq(Put(k, v))) }
}

final case class Operation[R](agg: Agg[R], commands: Seq[Command[R]], histories: Seq[History]) {
  def add(cmd: Command[R]): Operation[R] = copy(commands = commands :+ cmd)
  def execute: Result[R] = commands.foldLeft(Result(agg, Seq.empty)) { case (Result(a, hs), cmd) =>
    a.execute(cmd.asInstanceOf[a.Cmd]).pipe(e => Result(e.agg, hs ++ e.histories))
  }
}
final case class Result[R](agg: Agg[R], histories: Seq[History])
object Operation {
  def apply[R](executed: Executed[R], commands: Seq[Command[R]]): Operation[R] =
    apply(executed.agg, commands, executed.histories)
}

trait Command[R]

sealed trait History
final case class Created[I, R](id: I, root: R) extends History
final case class Added[K, E](key: K, entity: E) extends History
final case class Modified[E](entity: E) extends History
final case class Reset[E](entity: E) extends History
final case class Put[K, E](key: K, entity: E) extends History
final case class Removed[K](key: K) extends History
final case class Existed[E](e: E) extends History
final case class Emptied[E]() extends History

trait Ent[R, E] {
  val get: R => E
  val mod: R => (E => E) => R
  val set: R => E => R = r => e => mod(r)(_ => e)
}
object Ent {
  def apply[R, E](getter: R => E)(modifier: R => (E => E) => R): Ent[R, E] = new Ent[R, E] {
    override val get: R => E = getter
    override val mod: R => (E => E) => R = modifier
  }
}
trait EntO[R, E] {
  val empty: R => R
  val exist: R => E => R
}
object EntO {
  def apply[R, E](getter: R => Option[E])(modifier: R => (Option[E] => Option[E]) => R): EntO[R, E] = new EntO[R, E] {
    override val exist: R => E => R = r => e => modifier(r)(_ => Some(e))
    override val empty: R => R = r => modifier(r)(_ => None)
  }
}
trait EntM[R, K, E] {
  val put: R => K => E => R
}
object EntM {
  def apply[R, K, E](mapperR: R => Map[K, E])(mapperW: R => Map[K, E] => R): EntM[R, K, E] = new EntM[R, K, E] {
    override val put: R => K => E => R = r => k => v => mapperW(r)(mapperR(r).updated(k, v))
  }
}
trait Repository[F[_], I, R] {
  def store(id: I, result: Result[R]): F[I]
  def resolve(id: I): F[Option[R]]
  def lock(id: I): F[Either[LockError, R]]
}
object Repository {
  sealed trait LockError
  case object Locked extends LockError
  case object NotFound extends LockError
}
