package macroutils

//inline def v = debug(1 + 2)

@main
def main() = {

  lazy val x = 1
  debug((x + 2) - 1)
  debug(List(1, 2, 3).size)
//  println(v)
//  println(inspect(1 + 2))
}