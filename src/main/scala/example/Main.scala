package example

import java.nio.file.Paths

import cats.implicits._
import cats.effect._
import com.tozny.e3db._
import fs2.{Stream, io, text}

import e3dbs.E3dbs
import e3dbs.QueryResponseOps._
import e3dbs.MetaSplitter._

object Main extends MyApp with App {
  val program = for {
    user        <- prompt("Enter user name:")
    toznyConfig <- getConfigOrCreateUser(user)
    client = new ClientBuilder().fromConfig(toznyConfig).build()
    conference <- prompt("Enter conference name:")
    s          <- programLoop(client, conference)
  } yield s

  program
    .handleErrorWith(e => writeLine(s"Error: ${e.getMessage}"))
    .compile
    .drain
    .unsafeRunSync
}

trait MyApp {
  // In order to register "users", you'll need to get a client registration token.
  // See the readme for more information.
  private val token = "<your token goes here>"
  private val host  = "https://api.e3db.com"

  def programLoop(client: Client, conference: String): Stream[IO, Unit] = {
    def helper(action: String) =
      if (action == "s") writeLine("Sharing not implemented...") ++ programLoop(client, conference)
      else if (action == "a") writeContact(client, conference) ++ programLoop(client, conference)
      else if (action == "l") listContacts(client, conference) ++ programLoop(client, conference)
      else if (action == "q") writeLine("Quiting..")
      else writeLine("What?") ++ programLoop(client, conference)
    for {
      action <- prompt("(S)hare, (A)dd contact, (L)ist contacts, (Q)uit")
      next   <- helper(action)
    } yield next
  }

  val readLine             = Stream.eval(IO(Console.in.readLine()))
  def writeLine(s: String) = Stream.eval(IO(println(s)))
  def prompt(s: String)    = writeLine(s) *> readLine

  def saveConfig(filename: String, config: Config): Stream[IO, Unit] =
    Stream
      .eval(IO(config.json()))
      .through(text.utf8Encode)
      .through(io.file.writeAll(Paths.get(filename))) ++ Stream.eval(IO(()))

  def getConfigOrCreateUser(username: String) =
    getConfig(username).handleErrorWith(
      _ =>
        prompt("Invalid user. Create new one? (Y/n)").flatMap(s =>
          if (s.isEmpty || s.toUpperCase == "Y") registerAndSave(username)
          else Stream()))

  def registerAndSave(username: String): Stream[IO, Config] =
    for {
      config <- Stream.eval(E3dbs.register(token, username, host))
      _      <- saveConfig(username + ".json", config)
    } yield config

  def getConfig(username: String): Stream[IO, Config] =
    io.file
      .readAll[IO](Paths.get(username + ".json"), 1024)
      .through(text.utf8Decode)
      .map(s => Config.fromJson(s))
      .handleErrorWith(_ => Stream.raiseError(new Exception(s"Invalid user $username")))

  def writeContact(client: Client, conference: String) = {
    val stream = for {
      name       <- prompt("Enter contact name:")
      company    <- prompt("Enter company name:")
      email      <- prompt("Enter email:")
      phone      <- prompt("Enter phone number:")
      confidence <- prompt("Enter confidence:")
      contact = Contact(name, company, email, phone, conference, confidence.toInt)
      record <- Stream.eval(E3dbs.write[Contact, ContactMeta](client, conference, contact))
      s      <- writeLine(s"Id: ${record.meta().recordId()}")
    } yield s
    stream.handleErrorWith(e => writeLine(s"Error creating contact: ${e.getMessage}"))
  }

  def listContacts(client: Client, conference: String) = {
    val params = new QueryParamsBuilder().setTypes(conference).setIncludeData(true).build()
    val stream = for {
      queryResponse <- Stream.eval(E3dbs.query(client, params))
      contact       <- Stream.emits(queryResponse.extract[Contact])
    } yield println(contact)
    stream.handleErrorWith(e => writeLine(s"Error ${e.getMessage()}"))
  }
}

// The case class I will use as the data to store in e3db. The data we send to
// e3db is encrypted/decrypted here on the client. However, e3db has the concept of
// metadata, which is NOT encrypted. The idea is that all personally identifying
// information (PII) should be encrypted. However, this means that you can not query based
// on it. So, NON-PII can be stored as metadata and used in the query. For example, you
// might put a zip code in the metadata so you can narrow down your results.
// The API essentially takes 2 Java Maps - one for the encrypted data and one for the
// metadata.
// First of all, I didn't want client code to have to know about or care about
// Java Maps. So I used shapeless for automatic typeclass derivation to convert
// case classes into HashMaps. These typeclasses are MapEncoder and MapDecoder.
// I also didn't want separate case classes for the data and the metadata because
// from the standpoint of the application, it's all data. So, I settled on being
// able to create a case class that is just used to distinguish what parts of a
// different case class is metadata. The fields in the metadata case class must be
// a subset of the fields in the data case class and in the same order, or there
// will be a compilation error. You can see the usage in writeContact above in the call to
// E3dbs.write.
case class Contact(name: String,
                   company: String,
                   email: String,
                   phone: String,
                   conference: String,
                   confidence: Int)

case class ContactMeta(conference: String, confidence: Int)
