package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] =
    for {
      intVal <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(intVal, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum of heap with two elements") = forAll { (int1: Int, int2: Int) =>
    val heap = insert(int1, insert(int2, empty))
    findMin(heap) == math.min(int1, int2)
  }

  property("deleting element from one-element heap results in empty heap") = forAll { (intVal: Int) =>
    deleteMin(insert(intVal, empty)) == empty
  }


  property("continuous deleting heap's elements results in sorted sequence") = forAll{ (heap: H) =>
    def getElemF(heapF: H): List[Int] = {
      if(isEmpty(heapF)) Nil
      else findMin(heapF) :: getElemF(deleteMin(heapF))
    }

    val heapElems = getElemF(heap)
    heapElems == heapElems.sorted
  }

  property("Compare if melding heap and heap2 is equal to melding heap1 with minimum element of heap2 and heap2 without min element ") = forAll { (h1: H, h2: H) =>
    def getElemF(heapF: H): List[Int] = {
      if (isEmpty(heapF)) Nil
      else findMin(heapF) :: getElemF(deleteMin(heapF))
    }

    getElemF(meld(h1, h2)) == getElemF(meld(deleteMin(h1), insert(findMin(h1), h2)))

  }


  property("meld1") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = findMin(meld(h1, h2))
    if(min1 > min2) min == min2 else min == min1
  }


}

