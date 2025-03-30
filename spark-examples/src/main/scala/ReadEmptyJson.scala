import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.col

object ReadEmptyJson extends App {

  val spark = SparkSession.builder.appName("Simple Application").master("local").getOrCreate()
  val r = spark.read.json("src/main/resources/*.{json,csv}").selectExpr("casd")
  println(r.count())
  r.show()
}
