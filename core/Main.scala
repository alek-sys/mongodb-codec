import com.alexnesterov._
import com.mongodb.ServerAddress
import com.mongodb.connection.{ClusterConnectionMode, ClusterType, ClusterSettings}
import org.bson.{BsonWriter, BsonReader}
import org.bson.codecs.{EncoderContext, DecoderContext, Codec}
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}
import org.mongodb.scala.{MongoClientSettings, MongoClient}

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

@CreateCodec
case class TestClass(username: String, age: Int)

object Hello {

  val clusterSettings: ClusterSettings =
    ClusterSettings.builder()
      .hosts(List[ServerAddress](new ServerAddress("localhost", 27017)).asJava)
      .requiredClusterType(ClusterType.STANDALONE)
      .mode(ClusterConnectionMode.SINGLE)
      .build()

  def getClientSettings(codecs: Codec[_]*) = {

    val codecRegistry = CodecRegistries.fromRegistries(
      MongoClient.DEFAULT_CODEC_REGISTRY,
      CodecRegistries.fromCodecs(codecs.asJava)
    )

    MongoClientSettings.builder()
      .codecRegistry(codecRegistry)
      .clusterSettings(clusterSettings)
      .build()
  }


  def main(args: Array[String]) = {
    val clientSettings = getClientSettings(new TestClass.MongoCodec())
    val client = MongoClient(clientSettings)

    val users = client.getDatabase("test").getCollection[TestClass]("users")

    val res = Await.result(users.insertOne(TestClass("Test name", 30)).toFuture(), Duration(10, "second"))

    client.close()

    ()
  }
}