# e3db-scala-example
An example of wrapping a callback based Java API in a purely functional way.

At a recent job interview, I was asked to create a Scala console application using the company's Java API. Since one
of the requirements in the job posting was "*You’re an excellent functional programmer in a modern language, preferably 
Scala*", I opted to use [Cats Effect](https://typelevel.org/cats-effect/) and 
[fs2](https://functional-streams-for-scala.github.io/fs2/) to create a purely functional console application. I'm not 
sure why they chose to make a callback based API rather than use Java CompletableFutures - compatibility with older versions
of Java, I suppose. I didn't get the job, but I thought others may be interested in an example of how to use callback 
based APIs in a purely functional way. 

The product in question is Innovault/E3DB by [Tozny](http://tozny.com), which is an end-to-end encrypted NoSQL datastore.
An interesting aspect of the database is that data is divided into encryted and metadata (unencrypted) portions. 
All Personally Identifiable Information should be encrypted, but non-PII metadata is left unencrypted so that it can be used to 
query the database. Both pieces of data are provided to the API using Java mutable Maps. Due to time constraints, during the 
interview, I used methods in a case class companion object to create the Maps from a case class, and convert a Map back to a 
case class. 

However, having to use non-typesafe Map[String, String] at all was bad enough, but the need to create them for each case 
class was even more unpleasant. So, when I got home, I extended the example by creating typeclass to convert to/from case 
classes and Maps, and used [shapeless](https://github.com/milessabin/shapeless) to automatically derive instances for these 
typeclasses. I alsocame up with a way to split the case class into data and metadata Maps. It's not perfect, but it is 
typesafe. See comments in Main.cs for more information.

In order to actually run this example, you would need to sign up for a free account at https://console.tozny.com/, then
create a **Client Registration Token** in the console. Paste that token into the *token* val in Main.cs.