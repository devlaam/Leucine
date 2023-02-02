package s2a.leucine.actors

/** Trait that exposes a method to cancel some process. */
trait Cancellable :
  /** Tries its best to cancel the scheduled task. */
  def cancel(): Unit
  /** See if this a dummy Cancellable */
  def isEmpty = this == Cancellable.empty

object Cancellable :
  /** The dummy Cancellable */
  val empty = new Cancellable { def cancel() = () }

