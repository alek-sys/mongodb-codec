import com.alexnesterov._

@CreateCodec
case class TestClass()

object Hello extends App {
  val t = TestClass()
  println(classOf[TestClass.Codec])
}