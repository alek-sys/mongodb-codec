package com.alexnesterov

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

object MongoCodecProvider {

  def getCodec[T](): Any = macro MongoCodecProvider.getCodecImpl[T]

  def getCodecImpl[T: c.WeakTypeTag](c: whitebox.Context)(): c.Expr[Any] = {
    import c.universe._

    val fields = weakTypeOf[T].members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m
    } toList

    val methodTypePostfix = (f: MethodSymbol) => f.returnType.toString match {
      case "Long" => "Int64"
      case "Int" => "Int32"
      case "String" => "String"
    }

    val writers = fields map(f => {
      val methodName = TermName(s"write${methodTypePostfix(f)}")
      q"""writer.$methodName(${Literal(Constant(f.name.toString))}, value.${f.name})  """
    })

    val readers = fields map(f => {
      val methodName = TermName(s"read${methodTypePostfix(f)}")
      val fieldName = Literal(Constant(f.name.toString))
      val paramName = f.name.toTermName
      q""" $paramName = reader.$methodName($fieldName) """
    })

    val className = weakTypeOf[T].typeSymbol.name.toTypeName
    val codecClassName = TypeName(className.toString + "Codec")

    c.Expr[Any](
      q""" new Codec[$className] {
            override def decode(reader: BsonReader, decoderContext: DecoderContext): $className = {
              reader.readStartDocument()
              reader.readObjectId()
              val result = new $className(..$readers)
              reader.readEndDocument()

              result
            }
            override def encode(writer: BsonWriter, value: $className, encoderContext: EncoderContext): Unit = {
              writer.writeStartDocument()
              ..$writers
              writer.writeEndDocument()
            }
            override def getEncoderClass: Class[$className] = classOf[$className]
           }
       """)
  }
}