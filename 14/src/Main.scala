object Main extends App {
  case class State(rec: List[Int], f: Int, s: Int)
  {
    def next: State = {
      val fv = rec(f)
      val sv = rec(s)
      val nrs = (fv + sv).toString.map(_.toInt).map(_ - '0'.toInt)
      val nrec = rec ++ nrs.toList
      def a(i: Int, v: Int): Int = (i + v + 1) % nrec.length
      println(nrec.length)
      State(nrec, a(f, fv), a(s, sv))
    }

    def render = {
      rec.zipWithIndex.foreach { case (r, i) =>
        val str = if (i == f)
          f"($r%d)" else if (i == s)
          f"[$r%d]" else s" $r "

        print(str)  }
      println()
    }
  }


  val test = State(List(3, 7), 0, 1)

  def comp(is: State, limit: Int): Unit = {
    val tr = Stream.iterate(is)(_.next).find(_.rec.length >= 10 + limit)

    tr.get.rec.drop(limit).take(10).foreach(print)
  }

  comp(test, 77201)



}
