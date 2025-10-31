import scala.quoted.*
import scala.annotation.StaticAnnotation
import scala.compiletime.{error, codeOf}
// Аннотации
class Validate(min: Int, max: Int) extends StaticAnnotation
class Loggable extends StaticAnnotation

class UserService {
  @Validate(min = 1, max = 100)
  @Loggable
  def getUserById(id: Int): String = s"User $id"

  @Loggable
  def createUser(name: String): String = s"Created $name"
}

object AnnotationUtils {
  inline def printMethodAnnotations[T](inline methodName: String): String =
    ${ printMethodAnnotationsCode[T]('methodName) }

  def printMethodAnnotationsCode[T: Type](
                                           methodName: Expr[String]
                                         )(using Quotes): Expr[String] = {
    import quotes.reflect.*

    val methodNameStr = methodName.valueOrError
    val classSymbol = TypeRepr.of[T].typeSymbol

    classSymbol.methodMember(methodNameStr).headOption match {
      case Some(methodSymbol) =>
        val annotations = methodSymbol.annotations.map(_.show)
        println(s"Annotations for $methodNameStr: ${annotations.mkString(", ")}")
        Expr(s"Annotations for $methodNameStr: ${annotations.mkString(", ")}")
      case None =>
        report.errorAndAbort(s"Method $methodNameStr not found in ${classSymbol.name}")
    }
  }
}
