package caliban

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContextExecutor, Future }
import scala.language.postfixOps
import java.util.concurrent.TimeUnit
import caliban.Data._
import caliban.GraphQL._
import io.circe.Json
import org.openjdk.jmh.annotations._
import sangria.execution._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.parser.QueryParser
import sangria.schema._
import zio.internal.Platform
import zio.{ BootstrapRuntime, Runtime, UIO, ZEnv }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class GraphQLBenchmarks {

  val complexQuery: String =
    """{
          foos {
            id
            name
            bars {
              id
              name
              baz {
                id
                name
              }
            }
          }
       }""".stripMargin

  val simpleQuery: String =
    """{
          characters{
            name
          }
       }""".stripMargin

  val simpleQueryWithZIOField: String =
    """{
          characters{
            nameZIO
          }
       }""".stripMargin

  val simpleQueryWithZQueryField: String =
    """{
          characters{
            nameZQuery
          }
       }""".stripMargin

  val fullIntrospectionQuery = """
              query IntrospectionQuery {
                __schema {
                  queryType { name }
                  mutationType { name }
                  subscriptionType { name }
                  types {
                    ...FullType
                  }
                  directives {
                    name
                    description
                    locations
                    args {
                      ...InputValue
                    }
                  }
                }
              }

              fragment FullType on __Type {
                kind
                name
                description
                fields(includeDeprecated: true) {
                  name
                  description
                  args {
                    ...InputValue
                  }
                  type {
                    ...TypeRef
                  }
                  isDeprecated
                  deprecationReason
                }
                inputFields {
                  ...InputValue
                }
                interfaces {
                  ...TypeRef
                }
                enumValues(includeDeprecated: true) {
                  name
                  description
                  isDeprecated
                  deprecationReason
                }
                possibleTypes {
                  ...TypeRef
                }
              }

              fragment InputValue on __InputValue {
                name
                description
                type { ...TypeRef }
                defaultValue
              }

              fragment TypeRef on __Type {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                    ofType {
                      kind
                      name
                      ofType {
                        kind
                        name
                        ofType {
                          kind
                          name
                          ofType {
                            kind
                            name
                            ofType {
                              kind
                              name
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
                """

  val runtime: Runtime[ZEnv] = new BootstrapRuntime {
    override val platform: Platform = Platform.benchmark
  }

  case class CharactersArgs(origin: Option[Origin])
  case class CharacterArgs(name: String)

  case class Query(
    characters: CharactersArgs => UIO[List[Character]],
    character: CharacterArgs => UIO[Option[Character]]
  )

  def resolver(characters: List[Character]): RootResolver[Query, Unit, Unit] = RootResolver(
    Query(
      args => UIO(characters.filter(c => args.origin.forall(c.origin == _))),
      args => UIO(characters.find(c => c.name == args.name))
    )
  )

  def makeInterpreter(characters: List[Character]): GraphQLInterpreter[Any, CalibanError] =
    runtime.unsafeRun(graphQL(resolver(characters)).interpreter)

  val interpreter = makeInterpreter(Data.characters)

  val interpreter1k = makeInterpreter(List.fill(100)(Data.characters).flatten)

  val interpreter10k = makeInterpreter(List.fill(1000)(Data.characters).flatten)

  val interpreter100k = makeInterpreter(List.fill(10000)(Data.characters).flatten)

  def queryCaliban(query: String, interpreter: GraphQLInterpreter[Any, CalibanError]): Unit = {
    val io = interpreter.execute(query)
    runtime.unsafeRun(io)
    ()
  }

  val complexInterpreter: GraphQLInterpreter[Any, CalibanError] =
    runtime.unsafeRun(graphQL(RootResolver(ComplexData.query)).interpreter)

  @Benchmark
  def complexCaliban(): Unit = queryCaliban(complexQuery, complexInterpreter)

  @Benchmark
  def complexSangria(): Unit = {
    import ComplexData._

    val future: Future[Json] =
      Future.fromTry(QueryParser.parse(complexQuery)).flatMap(queryAst => Executor.execute(SchemaDefinition, queryAst, (), deferredResolver = Resolver))
    Await.result(future, 1 minute)
    ()
  }

  @Benchmark
  def simpleCaliban(): Unit = queryCaliban(simpleQuery, interpreter)

  @Benchmark
  def simpleCalibanWithZIOField(): Unit = queryCaliban(simpleQueryWithZIOField, interpreter)

  @Benchmark
  def simpleCalibanWithZQueryField(): Unit = queryCaliban(simpleQueryWithZQueryField, interpreter)

  @Benchmark
  def simpleCaliban1k(): Unit = queryCaliban(simpleQuery, interpreter1k)

  @Benchmark
  def simpleCalibanWithZIOField1k(): Unit = queryCaliban(simpleQueryWithZIOField, interpreter1k)

  @Benchmark
  def simpleCalibanWithZQueryField1k(): Unit = queryCaliban(simpleQueryWithZQueryField, interpreter1k)

  @Benchmark
  def simpleCaliban10k(): Unit = queryCaliban(simpleQuery, interpreter10k)

  @Benchmark
  def simpleCalibanWithZIOField10k(): Unit = queryCaliban(simpleQueryWithZIOField, interpreter10k)

  @Benchmark
  def simpleCalibanWithZQueryField10k(): Unit = queryCaliban(simpleQueryWithZQueryField, interpreter10k)

  @Benchmark
  def simpleCaliban100k(): Unit = queryCaliban(simpleQuery, interpreter100k)

  @Benchmark
  def simpleCalibanWithZIOField100k(): Unit = queryCaliban(simpleQueryWithZIOField, interpreter100k)

  @Benchmark
  def simpleCalibanWithZQueryField100k(): Unit = queryCaliban(simpleQueryWithZQueryField, interpreter100k)

  @Benchmark
  def introspectCaliban(): Unit = {
    val io = interpreter.execute(fullIntrospectionQuery)
    runtime.unsafeRun(io)
    ()
  }

  implicit val OriginEnum: EnumType[Origin]             = deriveEnumType[Origin]()
  implicit val CaptainType: ObjectType[Unit, Captain]   = deriveObjectType[Unit, Captain]()
  implicit val PilotType: ObjectType[Unit, Pilot]       = deriveObjectType[Unit, Pilot]()
  implicit val EngineerType: ObjectType[Unit, Engineer] = deriveObjectType[Unit, Engineer]()
  implicit val MechanicType: ObjectType[Unit, Mechanic] = deriveObjectType[Unit, Mechanic]()
  implicit val RoleType: UnionType[Unit] = UnionType(
    "Role",
    types = List(PilotType, EngineerType, MechanicType, CaptainType)
  )
  implicit val CharacterType: ObjectType[Unit, Character] = ObjectType(
    "Character",
    fields[Unit, Character](
      Field(
        "name",
        StringType,
        resolve = _.value.name
      ),
      Field(
        "nicknames",
        ListType(StringType),
        resolve = _.value.nicknames
      ),
      Field(
        "origin",
        OriginEnum,
        resolve = _.value.origin
      ),
      Field(
        "role",
        OptionType(RoleType),
        resolve = _.value.role
      )
    )
  )

  val OriginArg: Argument[Option[Origin]] = Argument("origin", OptionInputType(OriginEnum))
  val NameArg: Argument[String]           = Argument("name", StringType)

  val QueryType: ObjectType[Unit, Unit] = ObjectType(
    "Query",
    fields[Unit, Unit](
      Field(
        "characters",
        ListType(CharacterType),
        arguments = OriginArg :: Nil,
        resolve = args => Future.successful(Data.characters.filter(c => (args arg OriginArg).forall(c.origin == _)))
      ),
      Field(
        "character",
        OptionType(CharacterType),
        arguments = NameArg :: Nil,
        resolve = args => Future.successful(Data.characters.find(c => c.name == (args arg NameArg)))
      )
    )
  )

  val schema: Schema[Unit, Unit] = Schema(QueryType)

  implicit val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  @Benchmark
  def simpleSangria(): Unit = {
    val future: Future[Json] =
      Future.fromTry(QueryParser.parse(simpleQuery)).flatMap(queryAst => Executor.execute(schema, queryAst))
    Await.result(future, 1 minute)
    ()
  }

  @Benchmark
  def introspectSangria(): Unit = {
    val future: Future[Json] =
      Future.fromTry(QueryParser.parse(fullIntrospectionQuery)).flatMap(queryAst => Executor.execute(schema, queryAst))
    Await.result(future, 1 minute)
    ()
  }

}
