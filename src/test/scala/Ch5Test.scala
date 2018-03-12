import org.scalatest.FunSuite

class Ch5Test extends FunSuite {

  def ints(n: Int): Stream[Int] = n #:: ints(n + 1)
  def squares(n: Int): Stream[Int] = (n*n) #:: ints(n + 1)

  test("testStreamToList") {
    assert(Ch5.streamToList(Stream(1,2,3,4,5)) == List(1,2,3,4,5))
  }

  test("testStreamToList1") {
    assert(Ch5.streamToList1(Stream(1,2,3,4,5)) == List(1,2,3,4,5))
  }

  test("test take on finite stream") {
    assert(Ch5.take(Stream(1,2,3,4,5), 3) == Stream(1,2,3))
  }

  test("test take on infinite stream") {
    assert(Ch5.take(ints(1), 3) == Stream(1,2,3))
  }

  test("test forAll on infinite stream on false case") {
    assert(Ch5.forAll(ints(1), (x: Int) => x < 1000) == false)
  }

  test("test forAll on finite stream on true case") {
    assert(Ch5.forAll(Stream(1,2,3,4,5), (x: Int) => x < 6) == true)
  }

  test("test map on infinite steam") {
    assert(Ch5.map(Stream(1,2,3,4), (x:Int) => x*x) == Stream(1,4,9,16))
  }

}
