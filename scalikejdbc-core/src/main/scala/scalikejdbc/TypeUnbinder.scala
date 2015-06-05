package scalikejdbc

import java.io.InputStream
import java.sql.PreparedStatement
import scalikejdbc.UnixTimeInMillisConverterImplicits._
import scala.language.experimental.macros
//import scala.reflect.macros.blackbox.Context

trait TypeUnbinder[A] { self =>

  def apply(value: A): ParameterBinder

  def contramap[B](f: B => A): TypeUnbinder[B] = new TypeUnbinder[B] {
    def apply(value: B): ParameterBinder = {
      if (value == null) ParameterBinder.NullParameterBinder
      else self(f(value))
    }
  }

}

object TypeUnbinder extends LowPriorityImplicitsTypeUnbinder1 {

  def apply[A](f: A => (PreparedStatement, Int) => Unit): TypeUnbinder[A] = new TypeUnbinder[A] {
    def apply(value: A): ParameterBinder = {
      if (value == null) ParameterBinder.NullParameterBinder
      else ParameterBinder(f(value))
    }
  }

  implicit val intTypeUnbinder: TypeUnbinder[Int] = TypeUnbinder { v => (ps, idx) => ps.setInt(idx, v) }
  implicit val stringTypeUnbinder: TypeUnbinder[String] = TypeUnbinder { v => (ps, idx) => ps.setString(idx, v) }
  implicit val sqlArrayTypeUnbinder: TypeUnbinder[java.sql.Array] = TypeUnbinder { v => (ps, idx) => ps.setArray(idx, v) }
  implicit val bigDecimalTypeUnbinder: TypeUnbinder[BigDecimal] = TypeUnbinder { v => (ps, idx) => ps.setBigDecimal(idx, v.bigDecimal) }
  implicit val booleanTypeUnbinder: TypeUnbinder[Boolean] = TypeUnbinder { v => (ps, idx) => ps.setBoolean(idx, v) }
  implicit val byteTypeUnbinder: TypeUnbinder[Byte] = TypeUnbinder { v => (ps, idx) => ps.setByte(idx, v) }
  implicit val sqlDateTypeUnbinder: TypeUnbinder[java.sql.Date] = TypeUnbinder { v => (ps, idx) => ps.setDate(idx, v) }
  implicit val doubleTypeUnbinder: TypeUnbinder[Double] = TypeUnbinder { v => (ps, idx) => ps.setDouble(idx, v) }
  implicit val floatTypeUnbinder: TypeUnbinder[Float] = TypeUnbinder { v => (ps, idx) => ps.setFloat(idx, v) }
  implicit val longTypeUnbinder: TypeUnbinder[Long] = TypeUnbinder { v => (ps, idx) => ps.setLong(idx, v) }
  implicit val shortTypeUnbinder: TypeUnbinder[Short] = TypeUnbinder { v => (ps, idx) => ps.setShort(idx, v) }
  implicit val sqlXmlTypeUnbinder: TypeUnbinder[java.sql.SQLXML] = TypeUnbinder { v => (ps, idx) => ps.setSQLXML(idx, v) }
  implicit val sqlTimeTypeUnbinder: TypeUnbinder[java.sql.Time] = TypeUnbinder { v => (ps, idx) => ps.setTime(idx, v) }
  implicit val sqlTimestampTypeUnbinder: TypeUnbinder[java.sql.Timestamp] = TypeUnbinder { v => (ps, idx) => ps.setTimestamp(idx, v) }
  implicit val urlTypeUnbinder: TypeUnbinder[java.net.URL] = TypeUnbinder { v => (ps, idx) => ps.setURL(idx, v) }
  implicit val utilDateTypeUnbinder: TypeUnbinder[java.util.Date] = sqlTimestampTypeUnbinder.contramap(_.toSqlTimestamp)
  implicit val jodaDateTimeTypeUnbinder: TypeUnbinder[org.joda.time.DateTime] = utilDateTypeUnbinder.contramap(_.toDate)
  implicit val jodaLocalDateTimeTypeUnbinder: TypeUnbinder[org.joda.time.LocalDateTime] = utilDateTypeUnbinder.contramap(_.toDate)
  implicit val jodaLocalDateTypeUnbinder: TypeUnbinder[org.joda.time.LocalDate] = sqlDateTypeUnbinder.contramap(_.toDate.toSqlDate)
  implicit val jodaLocalTimeTypeUnbinder: TypeUnbinder[org.joda.time.LocalTime] = sqlTimeTypeUnbinder.contramap(_.toSqlTime)
  implicit val inputStreamTypeUnbinder: TypeUnbinder[InputStream] = TypeUnbinder { v => (ps, idx) => ps.setBinaryStream(idx, v) }
  implicit val nullTypeUnbinder: TypeUnbinder[Null] = new TypeUnbinder[Null] { def apply(value: Null) = ParameterBinder.NullParameterBinder }
  implicit val noneTypeUnbinder: TypeUnbinder[None.type] = new TypeUnbinder[None.type] { def apply(value: None.type) = ParameterBinder.NullParameterBinder }

}

trait LowPriorityImplicitsTypeUnbinder1 extends LowPriorityImplicitsTypeUnbinder0 {

  implicit def optionalTypeUnbinder[A](implicit ev: TypeUnbinder[A]): TypeUnbinder[Option[A]] = new TypeUnbinder[Option[A]] {
    def apply(value: Option[A]): ParameterBinder = {
      if (value == null) ParameterBinder.NullParameterBinder
      else value.fold(ParameterBinder.NullParameterBinder)(ev.apply)
    }
  }

  def jsr310TypeUnbinder[A]: TypeUnbinder[A] = TypeUnbinder[A] { p =>
    (underlying, i) =>
      // Accessing JSR-310 APIs via Java reflection
      // because scalikejdbc-core should work on not only Java 8 but 6 & 7.
      import java.lang.reflect.Method
      val className: String = p.getClass.getCanonicalName
      val clazz: Class[_] = Class.forName(className)
      className match {
        case "java.time.ZonedDateTime" | "java.time.OffsetDateTime" =>
          val instant = clazz.getMethod("toInstant").invoke(p) // java.time.Instant
          val dateClazz: Class[_] = Class.forName("java.util.Date") // java.util.Date
          val fromMethod: Method = dateClazz.getMethod("from", Class.forName("java.time.Instant"))
          val dateValue = fromMethod.invoke(null, instant).asInstanceOf[java.util.Date]
          underlying.setTimestamp(i, dateValue.toSqlTimestamp)
        case "java.time.LocalDateTime" =>
          underlying.setTimestamp(i, org.joda.time.LocalDateTime.parse(p.toString).toDate.toSqlTimestamp)
        case "java.time.LocalDate" =>
          underlying.setDate(i, org.joda.time.LocalDate.parse(p.toString).toDate.toSqlDate)
        case "java.time.LocalTime" =>
          underlying.setTime(i, org.joda.time.LocalTime.parse(p.toString).toSqlTime)
      }
  }

}

trait LowPriorityImplicitsTypeUnbinder0 {
  //  def anyTypeUnbinder[A]: TypeUnbinder[A] = macro TypeUnbinderMacro.any[A]
}

private[scalikejdbc] object TypeUnbinderMacro {

  //  def any[A: c.WeakTypeTag](c: Context): c.Expr[TypeUnbinder[A]] = {
  //    import c.universe._
  //    val A = weakTypeTag[A].tpe
  //    if (A.toString.startsWith("java.time.")) c.Expr[TypeUnbinder[A]](q"scalikejdbc.TypeUnbinder.jsr310TypeUnbinder[$A]")
  //    else c.abort(c.enclosingPosition, s"Could not find an implicit value of the TypeUnbinder[$A].")
  //  }

}