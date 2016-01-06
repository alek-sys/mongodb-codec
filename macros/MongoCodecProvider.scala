package com.alexnesterov

import org.bson.Document
import org.bson.codecs.Codec

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

object MongoCodecProvider {

  def getCodec[T](): Any = macro MongoCodecProvider.getCodecImpl[T]

  def getCodecImpl[T: c.WeakTypeTag](c: whitebox.Context)(): c.Expr[Any] = {
    import c.universe._

    val mainType = weakTypeOf[T]

    def keyName(term: TermName) = Literal(Constant(term.toString))

    def getFields(t: Type): List[MethodSymbol] = t.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m
    } toList

    def isCaseClass(t: Type): Boolean = {
      t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass
    }

    def getCaseClasses(t: Type): List[Type] = {
      getFields(t) map({_.returnType}) filter(isCaseClass)
    }

    def getInstanceMethods(types: List[Type]): Tree = {

      q"()"
    }

    def getDocumentMethodName(t:Type) = TermName("getDocument" + t.typeSymbol.name.toString)

    def getCaseClassMethod(cls: c.universe.MethodSymbol): Tree = {
      q"document.put(${keyName(cls.name)}, ${getDocumentMethodName(cls.returnType)}(instance.${cls.name}))"
    }

    def getValueMethod(v: c.universe.MethodSymbol): Tree = {
      q"document.put(${keyName(v.name)}, instance.${v.name})"
    }

    def getDocumentMethod(t: c.Type): c.Tree = {
      val methodName = getDocumentMethodName(t)
      val fields = getFields(t)
      val putOps = fields.map{
        case c if isCaseClass(c.returnType) => getCaseClassMethod(c)
        case v => getValueMethod(v)
      }
      q"""def $methodName(instance: ${t.typeSymbol.asType.name}): Document = {
            val document = new Document()
            ..$putOps
            document
         }
       """
    }

    val allClasses = (mainType :: getCaseClasses(mainType)).distinct

    val getDocumentMethods = for( c <- allClasses) yield getDocumentMethod(c)

    val className = mainType.typeSymbol.name.toTypeName

    c.Expr[Any](
      q""" new BaseCodec[$className]() {
            ..$getDocumentMethods

            def getInstance(document: Document): $className = {
              null
            }

            def getDocument(instance: $className): Document = {
              ${getDocumentMethodName(mainType)}(instance)
            }
            override def getEncoderClass: Class[$className] = classOf[$className]
           }
       """)
  }
}