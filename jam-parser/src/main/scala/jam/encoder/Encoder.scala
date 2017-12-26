package jam.encoder

import jam.Yaml
import jam.Yaml._

trait Encoder[A] {

  def encode(v: A): Yaml
}

object Encoder extends EncoderDerivation {

  def instance[T](f: T => Yaml): Encoder[T] =
    f(_)

  implicit val booleanEncoder: Encoder[Boolean] =
    instance(if (_) YTrue else YFalse)

  implicit val stringEncoder: Encoder[String] =
    instance(YString)

  implicit val intEncoder: Encoder[Int] =
    instance(i => YBigDecimal(BigDecimal(i)))

  implicit val longEncoder: Encoder[Long] =
    instance(i => YBigDecimal(BigDecimal(i)))

  implicit val floatEncoder: Encoder[Float] =
    instance(i => YBigDecimal(BigDecimal.decimal(i)))

  implicit val doubleEncoder: Encoder[Double] =
    instance(i => YBigDecimal(BigDecimal.decimal(i)))

  implicit def optionEncoder[A](implicit enc: Encoder[A]): Encoder[Option[A]] =
    Encoder.instance {
      case None    => YNull
      case Some(t) => enc.encode(t)
    }

  implicit def seqEncoder[A](implicit enc: Encoder[A]): Encoder[Seq[A]] =
    Encoder.instance(t => YArray(t.map(enc.encode).toVector))
}
