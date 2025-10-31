
@main
def main(): Unit = {

  val x = AnnotationUtils.printMethodAnnotations[UserService]("getUserById")
  println("Abc")
}