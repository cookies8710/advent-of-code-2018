import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._

object Main extends App {
  val lines = Files.readAllLines(Paths.get("input")).asScala.map(java.lang.Long.parseLong)
  val result1 = lines.sum
  println(result1)

  var found = false
  var sums: Set[Long] = Set(0)
  var sum: Long = 0
  var result2: Option[Long] = None

  while (result2.isEmpty) {
    for (n <- lines) {
      sum += n
      if (sums.contains(sum) && result2.isEmpty)
        result2 = Some(sum)
      sums += sum
    }
  }

  println(result2)

}
