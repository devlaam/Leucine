package s2a.leucine.actors

import utest.*

object BurstQueueTest extends TestSuite :

  val tests = Tests {
    val queue = BurstQueue[Int]
    def check(size: Int, sum: Int, max: Int): Unit =
      queue.size ==> size
      queue.max  ==> max
      queue.sum  ==> sum
      queue.isEmpty ==> (size==0)
    test("Empty BurstQueue "){
      check(0,0,0)
      queue.dequeue() ==> List() }
    test("BurstQueue with 3 elements"){
      queue.enqueue(1)
      queue.enqueue(2)
      queue.enqueue(3)
      check(3,3,3)
      test("all dequeued"){
        queue.dequeue() ==> List(1,2,3)
        check(0,3,3) }
      test("all dequeued with tail"){
        queue.dequeue(List(4,5,6)) ==> List(1,2,3,4,5,6)
        check(0,3,3) }
      test("elements cleared"){
        queue.clear()
        check(0,3,3)
        test("and refilled"){
          queue.enqueue(4)
          queue.enqueue(5)
          check(2,5,3)
          test("and reset"){
            queue.reset()
            check(2,0,2) }
          test("all dequeued again"){
            queue.dequeue() ==> List(4,5)
            check(0,5,3) } } } } }


object DropQueueTest extends TestSuite :

  val tests = Tests {
    val queue = DropQueue[Int]
    def check(size: Int, sum: Int, max: Int): Unit =
      queue.size ==> size
      queue.max  ==> max
      queue.sum  ==> sum
      queue.isEmpty ==> (size==0)
    test("removeReverse-1"){
      val list = List(1,2,3,4,5,6)
      val result = DropQueue.removeReverse[Int](_%2==0,list)
      val expect = DropQueue.RemovedReversed[Int](List(5,3,1),3,true)
      result ==> expect }
    test("removeReverse-2"){
      val list = List(1,2,3,4,5,6)
      val result = DropQueue.removeReverse[Int](_=>true,list)
      val expect = DropQueue.RemovedReversed[Int](Nil,0,true)
      result ==> expect }
    test("removeReverse-3"){
      val list = List(1,2,3,4,5,6)
      val result = DropQueue.removeReverse[Int](_>6,list)
      val expect = DropQueue.RemovedReversed[Int](list.reverse,6,false)
      result ==> expect }
    test("removeReverse-4"){
      val list = List(1)
      val result = DropQueue.removeReverse[Int](_==1,list)
      val expect = DropQueue.RemovedReversed[Int](Nil,0,true)
      result ==> expect }
    test("removeReverse-5"){
      val list = Nil
      val result = DropQueue.removeReverse[Int](_==1,list)
      val expect = DropQueue.RemovedReversed[Int](Nil,0,false)
      result ==> expect }
    test("Empty DropQueue "){
      check(0,0,0)
      queue.dequeue ==> List() }
    test("BurstQueue with 1 element en/dequeue"){
      queue.enqueue(1)
      queue.dequeue ==> List(1)
      check(0,1,1) }
    test("BurstQueue with 1 element remove"){
      queue.enqueue(1)
      queue.remove(_==1)
      queue.dequeue ==> Nil
      check(0,1,1) }
    test("BurstQueue with 1 element remove"){
      queue.enqueue(1)
      queue.remove(_!=1)
      queue.dequeue ==> List(1)
      check(0,1,1) }
    test("BurstQueue with 6 elements"){
      queue.enqueue(1)
      queue.enqueue(2)
      queue.enqueue(3)
      queue.enqueue(4)
      queue.enqueue(5)
      queue.enqueue(6)
      check(6,6,6)
      test("one dequeued"){
        queue.dequeue ==> List(1)
        check(5,6,6) }
      test("three dequeued"){
        queue.dequeue ==> List(1)
        queue.dequeue ==> List(2)
        queue.dequeue ==> List(3)
        check(3,6,6)
        test("two requeued"){
          queue.enqueue(7)
          queue.enqueue(8)
          check(5,8,6)
          test("rest dequeued"){
            queue.dequeue ==> List(4)
            queue.dequeue ==> List(5)
            queue.dequeue ==> List(6)
            queue.dequeue ==> List(7)
            queue.dequeue ==> List(8)
            check(0,8,6) } } }
      test("even removal"){
        queue.remove(_%2==0)
        check(3,6,6)
        test("complete dequeing"){
          queue.dequeue ==> List(1)
          queue.dequeue ==> List(3)
          queue.dequeue ==> List(5) }
        test("multiples of 3 removed"){
          queue.remove(_%3==0)
          queue.dequeue ==> List(1)
          queue.dequeue ==> List(5) }
        test("3 elements requeued"){
          queue.enqueue(2)
          queue.enqueue(4)
          queue.enqueue(6)
          check(6,9,6)
          test("multiples of 3 removed"){
            queue.remove(_%3==0)
            queue.dequeue ==> List(1)
            queue.dequeue ==> List(5)
            queue.dequeue ==> List(2)
            queue.dequeue ==> List(4)
            queue.dequeue ==> Nil }
          test("multiples of 4 removed"){
            queue.remove(_%4==0)
            queue.dequeue ==> List(1)
            queue.dequeue ==> List(3)
            queue.dequeue ==> List(5)
            queue.dequeue ==> List(2)
            queue.dequeue ==> List(6)
            queue.dequeue ==> Nil }
          test("multiples of 5 removed"){
            queue.remove(_%5==0)
            queue.dequeue ==> List(1)
            queue.dequeue ==> List(3)
            queue.dequeue ==> List(2)
            queue.dequeue ==> List(4)
            queue.dequeue ==> List(6)
            queue.dequeue ==> Nil }
          test("multiples of 7 removed"){
            queue.remove(_%7==0)
            queue.dequeue ==> List(1)
            queue.dequeue ==> List(3)
            queue.dequeue ==> List(5)
            queue.dequeue ==> List(2)
            queue.dequeue ==> List(4)
            queue.dequeue ==> List(6)
            queue.dequeue ==> Nil } } } } }


object StackQueueTest extends TestSuite :

  val tests = Tests {
    val queue = StackQueue[Int](9)
    def check(size: Int, sum: Int, max: Int): Unit =
      queue.size ==> size
      queue.max  ==> max
      queue.sum  ==> sum
      queue.isEmpty ==> (size==0)
    test("Empty StackQueue "){
      check(0,0,0)
      queue.head ==> 9 }
    test("StackQueue with 6 elements"){
      queue.enqueue(1)
      queue.enqueue(2)
      queue.enqueue(3)
      queue.enqueue(4)
      queue.enqueue(5)
      check(5,5,5)
      test("all popped "){
        queue.head ==> 5
        queue.pop(1)
        queue.head ==> 4
        queue.pop(2)
        queue.head ==> 2
        queue.pop(4)
        queue.head ==> 9
        check(0,5,5) }
      // test("all dequeued with tail"){
      //   queue.dequeue(List(4,5,6)) ==> List(1,2,3,4,5,6)
      //   check(0,3,3) }
      // test("elements cleared"){
      //   queue.clear()
      //   check(0,3,3)
      //   test("and refilled"){
      //     queue.enqueue(4)
      //     queue.enqueue(5)
      //     check(2,5,3)
      //     test("and reset"){
      //       queue.reset()
      //       check(2,0,2) }
      //     test("all dequeued again"){
      //       queue.dequeue() ==> List(4,5)
      //       check(0,5,3) } } }
} }
