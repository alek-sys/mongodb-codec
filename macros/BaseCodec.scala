package com.alexnesterov

import org.bson.{BsonWriter, BsonReader}
import org.bson.codecs.{EncoderContext, DecoderContext, Codec}
import org.bson.Document
import org.mongodb.scala.MongoClient

abstract class BaseCodec[T](documentCodec: Codec[Document]) extends Codec[T] {
  def this() = this(MongoClient.DEFAULT_CODEC_REGISTRY.get(classOf[Document]))

  def getInstance(document: Document): T
  def getDocument(instance: T): Document

  override def decode(reader: BsonReader, decoderContext: DecoderContext): T = {
    val document = documentCodec.decode(reader, decoderContext)
    getInstance(document)
  }

  override def encode(writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit = {
    val document = getDocument(value)
    documentCodec.encode(writer, document, encoderContext)
  }
}