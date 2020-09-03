package caliban

import zio.query._
import zio._

object Data {
  sealed trait Origin

  case object EARTH extends Origin
  case object MARS  extends Origin
  case object BELT  extends Origin

  sealed trait Role

  case class Captain(shipName: String)  extends Role
  case class Pilot(shipName: String)    extends Role
  case class Engineer(shipName: String) extends Role
  case class Mechanic(shipName: String) extends Role

  case class Character(
    name: String,
    nameZIO: ZIO[Any, Nothing, String],
    nameZQuery: ZQuery[Any, Nothing, String],
    nicknames: List[String],
    origin: Origin,
    role: Option[Role]
  )

  val characters = List(
    Character(
      "James Holden",
      ZIO.succeed("James Holden"),
      ZQuery.succeed("James Holden"),
      List("Jim", "Hoss"),
      EARTH,
      Some(Captain("Rocinante"))
    ),
    Character(
      "Naomi Nagata",
      ZIO.succeed("Naomi Nagata"),
      ZQuery.succeed("Naomi Nagata"),
      Nil,
      BELT,
      Some(Engineer("Rocinante"))
    ),
    Character(
      "Amos Burton",
      ZIO.succeed("Amos Burton"),
      ZQuery.succeed("Amos Burton"),
      Nil,
      EARTH,
      Some(Mechanic("Rocinante"))
    ),
    Character(
      "Alex Kamal",
      ZIO.succeed("Alex Kamal"),
      ZQuery.succeed("Alex Kamal"),
      Nil,
      MARS,
      Some(Pilot("Rocinante"))
    ),
    Character(
      "Chrisjen Avasarala",
      ZIO.succeed("Chrisjen Avasarala"),
      ZQuery.succeed("Chrisjen Avasarala"),
      Nil,
      EARTH,
      None
    ),
    Character(
      "Josephus Miller",
      ZIO.succeed("Josephus Miller"),
      ZQuery.succeed("Josephus Miller"),
      List("Joe"),
      BELT,
      None
    ),
    Character(
      "Roberta Draper",
      ZIO.succeed("Roberta Draper"),
      ZQuery.succeed("Roberta Draper"),
      List("Bobbie", "Gunny"),
      MARS,
      None
    ),
    Character(
      "Camina Drummer",
      ZIO.succeed("Camina Drummer"),
      ZQuery.succeed("Camina Drummer"),
      Nil,
      BELT,
      None
    ),
    Character(
      "Elvi Okoye",
      ZIO.succeed("Elvi Okoye"),
      ZQuery.succeed("Elvi Okoye"),
      Nil,
      EARTH,
      None
    ),
    Character(
      "Annushka Volovodov",
      ZIO.succeed("Annushka Volovodov"),
      ZQuery.succeed("Annushka Volovodov"),
      List("Anna"),
      EARTH,
      None
    )
  )
}
