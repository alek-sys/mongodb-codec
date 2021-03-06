import java.util.Date

import com.alexnesterov._
import com.mongodb.ServerAddress
import com.mongodb.connection.{ClusterConnectionMode, ClusterType, ClusterSettings}
import org.bson.types.ObjectId
import org.bson.{BsonWriter, BsonReader, Document}
import org.bson.codecs.{EncoderContext, DecoderContext, Codec}
import org.bson.codecs.configuration.{CodecRegistries}
import org.mongodb.scala.{MongoClientSettings, MongoClient}

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class Contacts(phone: String)
case class User(username: String, age: Int, DateOfBirth: Date, hobbies: java.util.List[String], contacts: Contacts, contact2: Contacts)

object Core {

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

  def main(args: Array[String]): Unit = {
    val codec = MongoCodecProvider.getCodec[User]()

    val clientSettings = getClientSettings(codec)
    val client = MongoClient(clientSettings)

    val users = client.getDatabase("test").getCollection[User]("users")

    Await.result(
      users.insertOne(
        User(
          age = 30,
          username = "Test name",
          DateOfBirth = new Date(),
          hobbies = List[String]("hiking", "music").asJava,
          contacts = new Contacts("123 12314"),
          contact2 = new Contacts("234 234234")))
      .toFuture(), Duration(10, "second"))

    val found = Await.result(users.find[User]().toFuture(), Duration(10, "second")).head
    println(s"User is ${found.username}")

    client.close()

    ()
  }
}