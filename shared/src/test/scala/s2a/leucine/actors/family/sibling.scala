package s2a.leucine.actors


import utest.*

import s2a.control.{Buffer, Deferred}


/* Actor Structure:
 *  Grandma (one instance)
 *  - Mama (3 instances)
 *    - Child (optional)
 *      Son <: Child  (3 instances)
 *      Daughter <: Child (3 instances)
 */

trait SiblingCommon :
  given ac: ActorContext = ActorContext.system

  def start(): Unit
  var writeln: String => Unit  = (_: String) => ()
  var done: Option[() => Unit] = None

  val result = Set("s3-SH-m1","s2-SH-m1","s1-SH-m2","s1-SH-m1","s2-SH-m3","s2-SH-m2","s3-SH-m2","d1-DH-m1","d2-DH-m1","d3-DH-m1","d3-DH-m2","d1-DH-m2",
                   "s3-SH-m3","s1-SH-m3","d2-DH-m2","s2-MF-m1","d2-MF-m1","d1-DH-m3","d1-MF-m1","s3-MF-m1","d3-DH-m3","s1-MF-m1","d2-DH-m3","d3-MF-m1",
                   "s1-MF-m2","d3-MF-m2","d1-MF-m2","d2-MF-m2","d1-MF-m3","d3-MF-m3","s3-MF-m2","s2-MF-m2","d2-MF-m3","s2-MF-m3","s3-MF-m3","s1-MF-m3")

  val tests = Tests {
    val buffer = Buffer[String]
    val deferred = Deferred(buffer.readlns)
    writeln = buffer.writeln
    done = Some(deferred.done)
    start()
    deferred.await()
    test("ContainsAllElements")  - { deferred.compare(list => list.toSet ==> result) } }
