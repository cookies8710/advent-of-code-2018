import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Main extends App {

  val lines = Files.readAllLines(Paths.get("input")).asScala
  val testLines = List("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

  case class Node(children: List[Node], metadata: List[Int])
  {
    def metadataSum: Int = metadata.sum + children.map(_.metadataSum).sum
    def value: Int = if (children.isEmpty) metadataSum else metadata.filter(x => x > 0 && x <= children.length).map(i => children(i - 1).value).sum
  }

  object Node
  {
    def parse(str: String): Node = {
      val (rootNode, remainingData) = read(str.split(" ").map(Integer.parseInt).toStream)
      assert(rootNode.isDefined)
      assert(remainingData.isEmpty)
      rootNode.get
    }

    private def readNode(data: Stream[Int], nodesRead: List[Node]): (Stream[Int], List[Node]) = {
      val (newNode, remaining) = read(data)
      (remaining, nodesRead.+:(newNode.get))
    }

    private def read(s: Stream[Int]): (Option[Node], Stream[Int]) = {
      if (s.isEmpty)
        (None, Stream.Empty)
      else {
        val (header, dataWithChildrenAndMetadata) = s.splitAt(2)
        val numChildren = header(0)
        val numMetadata = header(1)

        val (dataWithMetadata, children) = (0 until numChildren)
          .foldLeft((dataWithChildrenAndMetadata, List[Node]()))((remainingData, _) => readNode(remainingData._1, remainingData._2))

        val (metadata, remainingData) = dataWithMetadata.splitAt(numMetadata)
        (Some(Node(children.reverse, metadata.toList)), remainingData)
      }
    }
  }

  //val input = testLines(0)
  val input = lines(0)

  val root = Node.parse(input)

  val result1 = root.metadataSum
  println(s"result 1 = $result1")

  val result2 = root.value
  println(s"result 2 = $result2")
}
