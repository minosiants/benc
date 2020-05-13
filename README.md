## Benc
![build](https://github.com/minosiants/benc/workflows/build/badge.svg)  [![javadoc](https://javadoc.io/badge2/com.minosiants/benc_2.13/javadoc.svg)](https://javadoc.io/doc/com.minosiants/benc_2.13)   
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
libraryDependencies += "com.minosiants" %% "benc" % <version>
```


### Example 

```scala
  final case class Id(id: String)
  final case class Author(name: String, age: Option[Int])
  final case class Book(id: Id, author: Author, content: BitVector, pages: Long)
  val book = Book(...)
  //there are several ways to do conversion
   
  val bits:Either[Error, BitVector] = Benc.toBenc[Book](book)
  val backToBook:Either[Error, Book] = bits.flatMap(b => Benc.fromBenc[Book](b))

  //Using decoder adn encode directly
  BEncoder[Book].encode(book).flatMap(bt => BDecoder[Book].decode(bt))  

  //Using codec
  val codec = BCodec[Book]  
  codec.encode(book).flatMap(bt => codec.decode(bt))

  //Setting different name
  case class Pen(@BencKey("name") brand:String)
  
```

