package jam.encoder

import jam.Yaml
import jam.Yaml._
import org.scalatest.{ MustMatchers, WordSpec }

import scala.collection.immutable.ListMap

class EncoderTest extends WordSpec with MustMatchers {
  import EncoderTest._

  "Encoder" should {
    "encode primitives" in {
      val res: Yaml = Encoder[Primitives].encode(Primitives())
      res mustBe YMap(
        ListMap(
          "int"     -> YBigDecimal(1),
          "long"    -> YBigDecimal(2),
          "float"   -> YBigDecimal(3.3),
          "double"  -> YBigDecimal(4.4),
          "boolean" -> YTrue
        )
      )
    }

    "encode composed types" in {
      val c = Composed(Some(Primitives()), List(1, 2, -3), List("one", "two"))

      val res: Yaml = Encoder[Composed].encode(c)

      res mustBe YMap(
        ListMap(
          "primitives" -> YMap(
            ListMap(
              "int"     -> YBigDecimal(1),
              "long"    -> YBigDecimal(2),
              "float"   -> YBigDecimal(3.3),
              "double"  -> YBigDecimal(4.4),
              "boolean" -> YTrue
            )
          ),
          "numbers" -> YArray(Vector(YBigDecimal(1), YBigDecimal(2), YBigDecimal(-3))),
          "tags"    -> YArray(Vector(YString("one"), YString("two")))
        )
      )

      val res2: Yaml = Encoder[Composed].encode(c.copy(primitives = None, numbers = Nil))

      res2 mustBe YMap(
        ListMap(
          "primitives" -> YNull,
          "numbers"    -> YArray(Vector()),
          "tags"       -> YArray(Vector(YString("one"), YString("two")))
        )
      )
    }
  }
}

object EncoderTest {
  case class Primitives(
      int: Int = 1,
      long: Long = 2L,
      float: Float = 3.3f,
      double: Double = 4.4,
      boolean: Boolean = true
  )

  case class Composed(primitives: Option[Primitives], numbers: List[Long], tags: List[String])
}
