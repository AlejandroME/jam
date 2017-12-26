package jam.encoder

import jam.Yaml
import jam.Yaml._
import org.scalatest.{MustMatchers, WordSpec}

import scala.collection.immutable.ListMap

class EncoderTest extends WordSpec with MustMatchers {
  import EncoderTest._

  "Encoder" should {
    "encode primitives" in {
      val res: Yaml = Encoder[Primitives].encode(Primitives())
      res mustBe YMap(
        ListMap(
          "int" -> YBigDecimal(1),
          "long" -> YBigDecimal(2),
          "float" -> YBigDecimal(3.3),
          "double" -> YBigDecimal(4.4),
          "boolean" -> YTrue))
    }
  }
}

object EncoderTest {
  case class Primitives(int: Int = 1, long: Long = 2L, float: Float = 3.3f, double: Double = 4.4, boolean: Boolean = true)
}
