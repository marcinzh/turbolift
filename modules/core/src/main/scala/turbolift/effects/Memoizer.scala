package turbolift.effects
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.io.OnceVar


/** Signature for [[MemoizerEffect]].
 *
 * @tparam K input of the memoized function
 * @tparam V output of the memoized function
 */
trait MemoizerSignature[K, V] extends Signature:
  /** Invoke the function being memoized.
   *
   * The function being memoized is not specified, until the handler is called.
   */
  def memo(k: K): V !! ThisEffect

  /** Snapshot of the domain.
   *
   * The set of all arguments that `memo` has been called so far.
   */
  def domain: Set[K] !! ThisEffect

  /** Snapshot of the relation.
   *
   * The map from all arguments that `memo` has been called so far, to corresponding results.
   */
  def toMap: Map[K, V] !! ThisEffect


/** Base trait for custom instances of Memoizer effect.
 *
 * {{{
 * case object MyMemoizer extends MemoizerEffect[Int, String]
 * // optional:
 * type MyMemoizer = MyMemoizer.type
 * }}}
 *
 * Memoizes a recursive, effectful function.
 * Use the `memo` operation in places, where you'd normally want to invoke the function.
 * Provide the actual function as a parameter to handler.
 *
 * @see [[PolyMemoizerEffect]]
 * @see [[Memoizer]]
 *
 * @tparam K input of the memoized function
 * @tparam V output of the memoized function
 */
trait MemoizerEffect[K, V] extends Effect[MemoizerSignature[K, V]] with MemoizerSignature[K, V]:
  enclosing =>
  final override def memo(k: K): V !! this.type = perform(_.memo(k))
  final override def domain: Set[K] !! this.type = perform(_.domain)
  final override def toMap: Map[K, V] !! this.type = perform(_.toMap)

  final def apply(k: K): V !! this.type = memo(k)

  /** Predefined handlers for this effect. */
  object handlers:
    /** Backtrackable, but non-parallelizable. */
    def local[U](f: K => V !! (U & enclosing.type)): Handler[Identity, Identity, enclosing.type, U] =
      new impl.Stateful[Identity, Identity, U] with impl.Sequential with MemoizerSignature[K, V]:
        override type Local = Map[K, V]
        override def onInitial = Map().pure_!!
        override def onReturn(x: Unknown, s: Local) = x.pure_!!

        override def domain: Set[K] !! ThisEffect = Local.gets(_.keySet)
        override def toMap: Map[K, V] !! ThisEffect = Local.get
        override def memo(k: K): V !! ThisEffect =
          Local.get.flatMap: m =>
            m.get(k) match
              case Some(v) => !!.pure(v)
              case None =>
                for
                  v <- Control.reinterpret(f(k))
                  _ <- Local.modify(_.updated(k, v))
                yield v
      .toHandler


    /** Parallelizable, but non-backtrackable. */
    def shared[U <: IO](f: K => V !! (U & enclosing.type)): Handler[Identity, Identity, enclosing.type, U] =
      IO(new ConcurrentHashMap[K, OnceVar[V]]).flatMapHandler: storage =>
        new impl.Proxy[U] with MemoizerSignature[K, V]:
          override def domain: Set[K] !! ThisEffect = IO(storage.keySet().nn.asScala.toSet)

          override def toMap: Map[K, V] !! ThisEffect =
            IO(storage.entrySet().nn.iterator().nn.asScala).flatMap: entries =>
              entries.foldLeftEff(Map[K, V]()): (m, entry) =>
                val k = entry.getKey.nn
                val ovar = entry.getValue.nn
                ovar.get.map(v => m + ((k, v)))

          override def memo(k: K): V !! ThisEffect =
            IO:
              var wasFirst = false
              val ovar = storage.computeIfAbsent(k, _ => { wasFirst = true; OnceVar.unsafeCreate[V]() }).nn
              if wasFirst then
                Control.reinterpret(f(k)).tapEff(ovar.put)
              else
                ovar.get
            .flatten
        .toHandler


object MemoizerEffect:
  extension [K, V](thiz: MemoizerEffect[K, V])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler[U](f: K => V !! (U & thiz.type)): Handler[Identity, Identity, thiz.type, U] = thiz.handlers.local(f)

  //@#@TODO `fix` syntax doesn't work in Scala 3.6.3
  /*
  trait Fix[K, V, U] extends MemoizerEffect[K, V]:
    val handler: ThisHandler[Identity, Identity, U]

  def fix[K, V, U](f: (fx: Fix[K, V, U]) => K => V !! (U & fx.type)): Fix[K, V, U] = new:
    override val handler: ThisHandler[Identity, Identity, U] = handlers.default[U](f(this))
  */


/** Polymorphic variant of [[MemoizerEffect]].
 *
 * The 'K' and 'V' parameters are inferred from the call sites
 * of the effects's operations and handlers.
 */
abstract class PolyMemoizerEffect extends Effect.Polymorphic_-+[[X, Y] =>> MemoizerEffect[X, Y], Any, Any](new MemoizerEffect[Any, Any] {}):
  final def memo[K, V](k: K): V !! @@[K, V] = polymorphize[K, V].perform(_.memo(k))
  final def domain[K, V]: Set[K] !! @@[K, V] = polymorphize[K, V].perform(_.domain)
  final def toMap[K, V]: Map[K, V] !! @@[K, V] = polymorphize[K, V].perform(_.toMap)

  object handlers:
    def local[K, V, U](f: K => V !! (U & @@[K, V])): Handler[Identity, Identity, @@[K, V], U] =
      polymorphize[K, V]: p =>
        p.handler(_.handlers.local(k => p.lift(f(k))))

    def shared[K, V, U <: IO](f: K => V !! (U & @@[K, V])): Handler[Identity, Identity, @@[K, V], U] =
      polymorphize[K, V]: p =>
        p.handler(_.handlers.shared(k => p.lift(f(k))))


/** Predefined instance of [[PolyMemoizerEffect]] effect.
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object Memoizer extends PolyMemoizerEffect
type Memoizer[K, V] = Memoizer.@@[K, V]
