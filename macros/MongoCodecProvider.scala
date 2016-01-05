package com.alexnesterov

import org.bson.Document
import org.bson.codecs.Codec

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

object MongoCodecProvider {

  def getCodec[T](): Any = macro MongoCodecProvider.getCodecImpl[T]

  def getCodecImpl[T: c.WeakTypeTag](c: whitebox.Context)(): c.Expr[Any] = {
    import c.universe._

    val fields = weakTypeOf[T].members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m
    } toList

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
      q""" new BaseCodec[$className]() {
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