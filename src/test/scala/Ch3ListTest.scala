import org.scalatest.FunSuite

class Ch3ListTest extends FunSuite {

  test("tail is evoked on an non-empty list") {
    val l = List(1,2,3)
    assert(List(2,3) == Ch3List.tail(l))
  }

  test("tail is evoked on an empty list") {
    val l = List()
    assert(Ch3List.tail(l) == Nil)
  }

  test("drop is evoked on list larger than n") {
    val l = List(1,2,3,4,5,6)
    assert(Ch3List.drop(l,3) == List(4,5,6))
  }

  test("drop is evoked on list smaller than n") {
    val l = List(1,2,3)
    assert(Ch3List.drop(l,4) == Nil)
  }

  test("drop is evoked on empty list") {
    val l = List()
    assert(Ch3List.drop(l,4) == Nil)
  }

  def isEven(x: Int): Boolean = if (x%2 == 0) true else false

  test("dropWhile for all true") {
    val l = List(-2,0,4,6)
    assert(Ch3List.dropWhile(l,isEven) == Nil)
  }

  test("dropWhile for all false") {
    val l = List(-1,1,3,1)
    assert(Ch3List.dropWhile(l,isEven) == l)
  }

  test("dropWhile for valid input") {
    val l = List(1,2,3,4,5)
    assert(Ch3List.dropWhile(l,isEven) == List(1,3,5))
  }

  test("setHead on empty list") {
    val l = List()
    assert(Ch3List.setHead(l, 'a') == Nil)
  }

  test("setHead on valid list") {
    val l = List(1,2,3,4,5)
    assert(Ch3List.setHead(l,6) == List(6,2,3,4,5))
  }

  test("length on valid list") {
    val l = List(1,2,3,4)
    assert(Ch3List.length(l) == 4)
  }

  test("length on empty list") {
    val l = List()
    assert(Ch3List.length(l) == 0)
  }

  test("non-empty list is reversed") {
    val l = List(1,2,3)
    assert(Ch3List.reverseList2(l) == List(3,2,1))
  }

  test("empty list reversed returns Nil") {
    assert(Ch3List.reverseList2(List()) == Nil)
  }

  test("append two non-empty lists") {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    assert(Ch3List.append(l1, l2) == List(1,2,3,4,5,6))
  }

  test("concat non-empty list of lists") {
    assert(Ch3List.concat(List(List(1,2,3), List(3,4), List(7,8,9))) == List(1,2,3,3,4,7,8,9))
  }

  test("increment non-empty list of ints") {
    assert(Ch3List.increment(List(1,2,3)) == List(2,3,4))
  }

  test("increment empty list of ints") {
    assert(Ch3List.increment(List()) == Nil)
  }

  test("map square") {
    assert(Ch3List.map(List(1,2,3))(x => x*x) == List(1,4,9))
  }

  test("filter even") {
    def isEven(n: Int): Boolean = if (n%2 == 0) true else false
    assert(Ch3List.filter(List(1,2,3,4,5))(isEven) == List(2,4))
  }

  test("filter via flatmap even") {
    def isEven(n: Int): Boolean = if (n%2 == 0) true else false
    assert(Ch3List.filterViaFlatMap(List(1,2,3,4,5))(isEven) == List(2,4))
  }

  test("component add") {
    assert(Ch3List.componentAdd(List(1,2,3), List(4,5,6)) == List(5,7,9))
  }

  test("hasSubsequence on non-empty lists true") {
    assert(Ch3List.hasSubsequence(List(1,2,3,4,5), List(3,4)) == true)
  }

  test("hasSubsequence on non-empty lists false") {
    assert(Ch3List.hasSubsequence(List(1,2,3,4,5), List(3,4,6)) == false)
  }

  test("hasSubsequence on empty lists") {
    assert(Ch3List.hasSubsequence(List(), List(3,4,6)) == false)
  }

}
