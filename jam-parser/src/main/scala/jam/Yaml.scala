package jam

import jam.parser.YamlPrinter

import scala.collection.immutable.ListMap

sealed abstract class Yaml extends Product with Serializable {

  def isPrimitive: Boolean = true

  def print: String =
    YamlPrinter.printYaml(this, new StringBuilder).toString()
}

object Yaml {

  private[jam] final case object YNull extends Yaml

  private[jam] final case object YTrue extends Yaml

  private[jam] final case object YFalse extends Yaml

  private[jam] final case class YBigDecimal(v: BigDecimal) extends Yaml

  private[jam] final case class YString(v: String) extends Yaml

  private[jam] final case class YArray(v: Vector[Yaml]) extends Yaml {
    override def isPrimitive: Boolean = false
  }

  private[jam] final case class YMap(v: ListMap[String, Yaml]) extends Yaml {
    override def isPrimitive: Boolean = false

    def ++(m: YMap): YMap =
      YMap(v ++ m.v)
  }
}
