package io.yannick_cw.sjq

import org.scalatest.{FlatSpec, Matchers}
import io.circe.parser.parse

class jsonToCaseClassTest extends FlatSpec with Matchers {

  it should "parse json" in {
    val json = parse("""
        | {
        |   "name": "theName"
        | }
      """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(name: String)"))
  }

  it should "parse json with arrays" in {
    val json = parse("""
                       | {
                       |   "names": ["theName", "otherName"]
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(names: List[String])"))
  }

  it should "parse json with nested json objects" in {
    val json = parse("""
                       | {
                       |   "name": { "first": "Jo", "nr": 22 }
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(name: name)"),
                                        ("name", "case class name(first: String, nr: Double)"))
  }

  it should "parse json with double nested json objects" in {
    val json = parse("""
                       | {
                       |   "name": { "first": "Jo", "info": { "age": 22, "more": "more" } }
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(name: name)"),
                                        ("name", "case class name(first: String, info: info)"),
                                        ("info", "case class info(age: Double, more: String)"))
  }

  it should "parse json with null values" in {
    val json = parse("""
                       | {
                       |   "name": null
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(name: Option[Any])"))
  }

  it should "parse json empty lists" in {
    val json = parse("""
                       | {
                       |   "names": []
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(names: List[Any])"))
  }

  it should "parse json with nested objects in arrays" in {
    val json = parse("""
        |{  
        |  "hotels": { 
        |    "entities": [
        |      { "id": "1aa4c4ad-f9ea-3367-a163-8a3a6884d450", "name": "Dana Beach Resort" }
        |    ]
        |  }
        |}""".stripMargin)
      .fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(
      ("CC", "case class CC(hotels: hotels)"),
      ("hotels", "case class hotels(entities: List[entities])"),
      ("entities", "case class entities(id: String, name: String)")
    )
  }

  it should "parse json with different types in objects in arrays making them optional" in {
    val json = parse("""
                       | {
                       |   "list": [ { "id": 22 }, { "name": "Toben" } ]
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(list: List[list])"),
                                        ("list", "case class list(name: Option[String], id: Option[Double])"))
  }

  it should "parse json with different types in objects in arrays making only the always optional ones optional" in {
    val json = parse("""
                       | {
                       |   "list": [ { "id": 22 }, { "name": "Toben", "id": 12 } ]
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(list: List[list])"),
                                        ("list", "case class list(name: Option[String], id: Double)"))
  }

  it should "parse json with the same structure twice" in {
    val json = parse("""
                       | {
                       |   "sub": { "double": { "xx": "sds" } },
                       |   "another": { "double": { "xx": "sds" } }
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(
      ("CC", "case class CC(sub: sub, another: another)"),
      ("sub", "case class sub(double: double)"),
      ("double", "case class double(xx: String)"),
      ("another", "case class another(double: double)"),
    )
  }
}
