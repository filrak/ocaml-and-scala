
object Choose {
  def choose(inputStream: Stream[Any], from: Int, nth: Int): Stream[Any] = {
    def chooseIn(inputStream: Stream[Any], from: Int, nth: Int, iterator: Int): Stream[Any] = {
      if(iterator < from) {
        chooseIn(inputStream.tail, from, nth, iterator+1)
      } else {
        inputStream match {
          case Stream.Empty => Stream.Empty
          case h #:: tl => Stream.cons(h, chooseIn(tl, from, nth, iterator+1))
        }
      }
    }
    //Assumption: from =/= index and begins from 1
    chooseIn(inputStream, from, nth, 1)
  }

  def main(args: Array[String]) {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))))
    println(choose(s, 2, 3).toList)
  }
}
