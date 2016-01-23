package com.alexnesterov

import org.bson.Document
import org.bson.codecs.Codec

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

object MongoCodecProvider {

  def getCodec[T](): Codec[T] = macro MongoCodecProvider.getCodecImpl[T]

  def getCodecImpl[T: c.WeakTypeTag](c: whitebox.Context)(): c.Expr[Codec[T]] = {
    import c.universe._

    val mainType = weakTypeOf[T]

    def keyName(term: TermName) = Literal(Constant(term.toString))

    def getFields(t: Type): List[MethodSymbol] = t.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m
    } toList

    def isCaseClass(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass

    def getCaseClasses(t: Type): List[Type] = getFields(t).map(_.returnType).flatMap({t :: getCaseClasses(_)})

    def getMethodName(prefix: String)(t: Type) = TermName(s"get$prefix" + t.typeSymbol.name.toString)
    def getDocumentMethodName(t:Type) = getMethodName("Document")(t)
    def getInstanceMethodName(t:Type) = getMethodName("Instance")(t)

    def getDocumentMethod(t: c.Type): c.Tree = {
      val methodName = getDocumentMethodName(t)
      val putOps = getFields(t) map {
        case cls if isCaseClass(cls.returnType) => q"document.put(${keyName(cls.name)}, ${getDocumentMethodName(cls.returnType)}(instance.${cls.name}))"
        case v => q"document.put(${keyName(v.name)}, instance.${v.name})"
      }

      q"""def $methodName(instance: ${t.typeSymbol.asType.name}): org.bson.Document = {
            val document = new org.bson.Document()
            ..$putOps
            document
         }
       """
    }

    def getInstanceMethod(t: c.Type): c.Tree = {
      val methodName = getInstanceMethodName(t)
      val getOps = getFields(t) map { f =>
        val key = keyName(f.name)
        val accessor = f.name
        val getterMethodName = getInstanceMethodName(f.returnType)
        val getter = f match {
          case c if isCaseClass(c.returnType) => q"$getterMethodName(document.get($key).asInstanceOf[org.bson.Document])"
          case v => q"document.get($key).asInstanceOf[${f.returnType}]"
        }

        q"$accessor = $getter"
      }

      q"""def $methodName(document: org.bson.Document): ${t} = {
            new ${t.resultType}(..$getOps)
         }
       """
    }

    val allClasses = (mainType :: getCaseClasses(mainType)).distinct

    val getDocumentMethods = for( c <- allClasses) yield getDocumentMethod(c)
    val getInstanceMethods = for( c <- allClasses) yield getInstanceMethod(c)

    val className = mainType.typeSymbol.name.toTypeName

    c.Expr[Codec[T]](
      q""" new BaseCodec[$className]() {
            ..$getDocumentMethods
            ..$getInstanceMethods

            def getInstance(document: org.bson.Document): $className = {
              ${getInstanceMethodName(mainType)}(document)
            }

            def getDocument(instance: $className): org.bson.Document = {
              ${getDocumentMethodName(mainType)}(instance)
            }
            override def getEncoderClass: Class[$className] = classOf[$className]
           }
       """)
  }
}