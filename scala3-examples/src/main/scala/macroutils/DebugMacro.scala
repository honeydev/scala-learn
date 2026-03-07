package macroutils

import scala.quoted.*

def debugImpl(expr: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect.*

  val expression = expr.show
  '{
    val value = $expr
    println(${Expr(expression)} + " = " + value)
    value
  }

inline def debug(inline expr: Any): Any =
  ${ debugImpl('expr) }
