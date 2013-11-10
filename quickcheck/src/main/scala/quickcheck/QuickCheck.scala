package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll {a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("ordered") = forAll(genHeap, genHeap) {(h1: H, h2: H) =>
      val heap1 = meld(h1, h2)
      val min1 = findMin(h1)
      val heap2 = meld(deleteMin(h1), insert(min1, h2))
      heapOrder(heap1, List[A]()) == heapOrder(heap2, List[A]())
  }

  def heapOrder(h: H, list: List[A]): List[A] = h match {
    case t :: ts => {
      val min = findMin(h)
      heapOrder(deleteMin(h), min :: list)
    }
    case isEmpty => list
  }

  property("insert_delete") = forAll {a: Int =>
      deleteMin(insert(a, empty)) == empty
  }

  property("meld_heaps") = forAll(genHeap, genHeap) {(h1: H, h2: H) =>
      findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
