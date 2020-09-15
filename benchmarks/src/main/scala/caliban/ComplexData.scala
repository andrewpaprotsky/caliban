package caliban

import scala.concurrent.Future
import zio._
import zio.query._
import sangria.execution.deferred.{ DeferredResolver, Fetcher }
import sangria.execution.deferred.HasId
import sangria.schema._
import sangria.macros.derive._
import scala.concurrent.ExecutionContext.Implicits.global

object ComplexData {
  implicit val bazHasId: HasId[Baz, Int] = HasId(_.id)

  case class Foo(id: Int, name: String, bars: List[Bar])
  case class Bar(id: Int, name: String, bazId: Int, baz: ZQuery[Any, Throwable, Baz])
  case class Baz(id: Int, name: String)
  case class Query(foos: List[Foo])

  val Bazs = List.fill(1000)("baz").zipWithIndex.map { case (name, i) => Baz(i, name) }
  val Bars = List.fill(1000)("bar").zipWithIndex.map { case (name, i) => Bar(i, name, i, baz(i)) }
  val Foos = List.fill(10)("foo").zipWithIndex.zip(Bars.grouped(100).toList).map {
    case ((name, i), bars) => Foo(i, name, bars)
  }

  implicit val FooType: ObjectType[Unit, Foo] = deriveObjectType[Unit, Foo]()
  implicit def BarType: ObjectType[Unit, Bar] =
    ObjectType(
      "Bar",
      fields[Unit, Bar](
        Field("id", IntType, resolve = c => c.value.id),
        Field("name", StringType, resolve = c => c.value.name),
        Field("baz", BazType, resolve = c => bazsFetcher.defer(c.value.bazId))
      )
    )
  implicit val BazType: ObjectType[Unit, Baz] = deriveObjectType[Unit, Baz]()
  implicit val QueryType = ObjectType(
    "Query",
    fields[Any, Unit](
      Field("foos", ListType(FooType), resolve = c => Foos)
    )
  )

  sealed trait BazRequest extends Request[Throwable, Baz]
  object BazRequest {
    case class GetById(id: Int) extends BazRequest
  }

  val BazDataSource =
    DataSource.Batched.make[Any, BazRequest](BazRequest.toString) { requests =>
      ZIO
        .succeed(requests.foldLeft(Set.empty[Int]) { case (ids, BazRequest.GetById(id)) => ids + id })
        .map(bazs)
        .map(_.groupBy(_.id))
        .map {
          case items =>
            requests.foldLeft(CompletedRequestMap.empty) {
              case (result, request @ BazRequest.GetById(id)) =>
                result.insert(request)(items.get(id).flatMap(_.headOption).toRight(new Throwable))
            }
        }
    }

  def bazs(ids: Set[Int]): List[Baz] = Bazs.filter(baz => ids.contains(baz.id))

  def baz(id: Int): ZQuery[Any, Throwable, Baz] = ZQuery.fromRequest(BazRequest.GetById(id))(BazDataSource)

  val bazsFetcher = Fetcher((ctx: Any, ids: Seq[Int]) => Future(bazs(ids.toSet)))

  val SchemaDefinition = Schema(QueryType)

  val Resolver = DeferredResolver.fetchers(bazsFetcher)

  val query = Query(Foos)
}
