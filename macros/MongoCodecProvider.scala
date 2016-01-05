package com.alexnesterov

import org.bson.Document
import org.bson.codecs.Codec

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

object MongoCodecProvider {

  def getCodec[T](documentCodec: Codec[Document]): Any = macro MongoCodecProvider.getCodecImpl[T]

  def getCodecImpl[T: c.WeakTypeTag](c: whitebox.Context)(documentCodec: c.Expr[Codec[Document]]): c.Expr[Any] = {
    import c.universe._

    val fields = weakTypeOf[T].members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m
    } toList

    val methodTypePostfix = (f: MethodSymbol) => f.returnType match {
      case t if t =:= typeOf[Long] => "Int64"
      case t if t =:= typeOf[Int] => "Int32"
      case t if t =:= typeOf[String] => "String"
      case _ => c.abort(c.enclosingPosition, s"field of given type is not supported")
    }

    val keyName = (term: TermName) => Literal(Constant(term.toString))

    val writers = fields map { f =>
      q""" document.put(${keyName(f.name)}, instance.${f.name})  """
    }

    val readers = fields map { f =>
      q""" ${f.name} = document.get(${keyName(f.name)}).asInstanceOf[${f.returnType}] """
    }

    val className = weakTypeOf[T].typeSymbol.name.toTypeName
    val codecClassName = TypeName(className.toString + "Codec")

    c.Expr[Any](
      q""" new BaseCodec[$className](documentCodec) {
            def getInstance(document: Document): $className = {
              new $className(..$readers)
            }

            def getDocument(instance: $className): Document = {
              val document = new Document()
              ..$writers
              document
            }
            override def getEncoderClass: Class[$className] = classOf[$className]
           }
       """)
  }
}