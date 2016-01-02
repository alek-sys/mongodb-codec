package com.alexnesterov

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class CreateCodec() extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro MacroImpl.createCodec
}

object MacroImpl {
  def createCodec(c: whitebox.Context)(annottees: c.Expr[Any]*) = {

    import c.universe._

    val (className, classParams) = annottees.map(_.tree) match {
      case List(q"case class $name(..$params)") => (name, params)
      case _ => c.abort(c.enclosingPosition, "the annotation can only be used with case classes")
    }

    val getWriteMethodName = (t: c.universe.Tree) => t match {
      case tq"String" => "writeString"
      case tq"Int" => "writeInt32"
      case _ => c.abort(c.enclosingPosition, "this field type is not supported")
    }

    val getWriter = (d: ValDef) => {
      val method = TermName(getWriteMethodName(d.tpt))
      q"""bsonWriter.$method(${Literal(Constant(d.name.toString))}, t.${TermName(d.name.toString)})"""
    }

    val objName = TermName(className.toString)
    val writers = classParams.asInstanceOf[List[ValDef]].map(getWriter)

    c.Expr[Unit](
      q"""case class $className(..$classParams)
          object $objName {
            class MongoCodec() extends Codec[$className] {
              override def getEncoderClass: Class[$className] = classOf[$className]

              override def decode(bsonReader: BsonReader, decoderContext: DecoderContext): $className = {
                null
              }

              override def encode(bsonWriter: BsonWriter, t: TestClass, encoderContext: EncoderContext): Unit = {
                bsonWriter.writeStartDocument()
                ..$writers
                bsonWriter.writeEndDocument()
              }
            }
          }""")
  }
}
