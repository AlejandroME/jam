package jam.printer
import jam.Yaml._
import org.scalatest.{ MustMatchers, WordSpec }

import scala.collection.immutable.ListMap
import scala.io.Source

class YamlPrinterTest extends WordSpec with MustMatchers {

  "YamlPrinter" should {
    "generate yaml" in {
      val sb = new StringBuilder()
      val yaml = YMap(
        ListMap(
          "info" -> YMap(
            ListMap(
              "address"   -> YString("none"),
              "telephone" -> YString("12345"),
              "info" -> YMap(
                ListMap(
                  "address"   -> YString("none"),
                  "telephone" -> YString("12345")
                )
              )
            )
          ),
          "test" -> YBigDecimal(0)
        )
      )
      val value = YamlPrinter.printYaml(yaml, sb).toString()
      value mustBe getYaml("/nestedObject.yaml")

      val yaml2 = YMap(
        ListMap(
          "numbers" -> YArray(
            Vector(YBigDecimal(-1), YBigDecimal(2), YBigDecimal(3))
          ),
          "empty" -> YArray(Vector()),
          "details" -> YArray(
            Vector(
              YMap(
                ListMap(
                  "count"     -> YBigDecimal(-1),
                  "something" -> YTrue,
                  "array" -> YArray(
                    Vector(
                      YMap(
                        ListMap(
                          "count" -> YBigDecimal(1),
                          "name"  -> YString("james"),
                          "object" -> YMap(
                            ListMap(
                              "kind"   -> YString("nothing to see"),
                              "double" -> YBigDecimal(-0.001)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              YMap(ListMap("count" -> YBigDecimal(2)))
            )
          )
        )
      )
      val sb2    = new StringBuilder()
      val value2 = YamlPrinter.printYaml(yaml2, sb2).toString()
      value2 mustBe getYaml("/nestedArrays.yaml")
    }
  }

  def getYaml(path: String): String =
    Source
      .fromInputStream(this.getClass.getResourceAsStream(path))
      .mkString

}
