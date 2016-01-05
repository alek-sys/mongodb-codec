package com.alexnesterov

import org.bson.{BsonWriter, BsonReader}
import org.bson.codecs.{EncoderContext, DecoderContext, Codec}
import org.bson.Document

abstract class BaseCodec[T](documentCodec: Codec[Document]) extends Codec[T] {
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