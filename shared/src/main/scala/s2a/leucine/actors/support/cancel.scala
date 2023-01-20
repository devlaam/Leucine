package s2a.leucine.actors

trait Cancellable :
  /* Tries its best to cancel the scheduled task. */
  def cancel(): Unit
  def isEmpty = this == Cancellable.empty

object Cancellable :
  val empty = new Cancellable { def cancel() = () }
