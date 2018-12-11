object Main extends App {
  def powerLevel(x: Int, y: Int, serial: Int): Long = {
    def hundreds(i: Long): Long = if (i< 100) 0 else (i / 100) % 10
    hundreds(x*x*y + 20*x*y + x*serial + 100*y + 10*serial)-5
  }

  def grid(sn: Int): Map[(Int, Int), Long] =
    (for(x <- 1 to 300; y <- 1 to 300)
      yield (x, y) -> powerLevel(x, y, sn)).toMap

  def regionsum(x: Int, y: Int, g: Map[(Int, Int), Long], size: Int): Long =
    (for(i <- x until x + size; j <- y until y + size ) yield g(i -> j)).sum

  case class Solution(x: Int, y: Int, power: Long, size: Int)

  val serial = 2694
  def maxPowerlevel(grid: Map[(Int, Int), Long], size: Int): Solution = {
    val powerLevels = for(i <- 1 to 300 - size + 1; j <- 1 to 300 - size + 1) yield (i, j, regionsum(i, j, grd, size))
    val r = powerLevels.maxBy(_._3)
    Solution(r._1, r._2, r._3, size)
  }

  val grd: Map[(Int, Int), Long] = grid(2694)
  val result1 = maxPowerlevel(grd, 3)
  println(s"result1 : $result1")

  var prev = 0L
  var power = 0L
  var maxsol: Option[Solution] = None
  var maxpower: Option[Long] = None
  for(i <- 1 to 300; j <- 1 to 300; size <- 1 to 300)
  {
    if (j == 1 && size == 1) println(s"i = $i")
    if (size == 1) prev = 0
    if (i + size <= 300 && j + size <= 300)
    {
      power = if (size > 1) {
        val newrow = (i until i + size - 1).map(x => grd(x -> (j + size - 1))).toList
        val newcol = (j until j + size - 1).map(y => grd((i + size - 1) -> y)).toList
        val nrs = newrow.sum
        val ncl = newcol.sum
        prev + nrs + ncl + grd((i + size - 1) -> (j + size - 1))
      }
      else grd(i -> j)
      val newSol = Solution(i, j, power, size)
      if (maxpower.isEmpty || maxpower.get < power)
        {
          maxsol = Some(newSol)
          maxpower = Some(power)
        }
    }
    prev = power
  }

  println(s"result 2 : ${maxsol.get}")
}
