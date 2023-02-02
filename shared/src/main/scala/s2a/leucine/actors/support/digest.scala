package s2a.leucine.actors

/** Interface to expose a method that digests some input. This is the antogonist of Callable. */
trait Digestable[Input] :

  /**
   * A task that digests some input, but does not return a result. It is naturally aimed
   * at producing side effects. */
  def digest(input: Input): Unit

