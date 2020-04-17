## Benc
![build](https://github.com/minosiants/benc/workflows/build/badge.svg)  
Bencoding library for scala

### Overview
#### Bencoding
[Specification](https://wiki.theory.org/index.php/BitTorrentSpecification#Bencoding)  
`Bencoding` is a way to specify and organize data in a terse format. It supports the following types: byte strings, integers, lists, and dictionaries. 

`Bencoding` is used in .torrent files

### Benc functionality  

1. Encode case classes to bencoding  
2. Decode bencoding to case classes

### Usage

```scala
resolvers += "Github packages minosiants" at "https://maven.pkg.github.com/minosiants/_"

libraryDependencies += "com.minosiatns" %% "benc" % "0.1"
```


### Example 

```scala
  final case class Id(id: String)
  final case class Author(name: String, age: Option[Int])
  final case class Book(id: Id, author: Author, content: BitVector, pages: Long)
  val book = Book(...)
  BEncoder[Book].encode(book).flatMap(bt => BDecoder[Book].decode(bt))  
```

