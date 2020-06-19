package io.yannick_cw.sjq

import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class jsonToCaseClassTest extends AnyFlatSpec with Matchers {

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

  it should "parse json with number arrays" in {
    val json = parse("""
                       | {
                       |   "ids": [22, 23]
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(ids: List[Double])"))
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
                       |   "name": null,
                       |   "id": 22
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(name: Null, id: Double)"))
  }

  it should "parse json with null values and not null values for a field" in {
    val json = parse("""
                       | {
                       |   "things": [
                       |     { "id": null },
                       |     { "id": 22, "more": { "name": "Xx" } }
                       |   ]
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(
      ("CC", "case class CC(things: List[things])"),
      ("things", "case class things(more: Option[more], id: Option[Double])"),
      ("more", "case class more(name: String)")
    )
  }

  it should "parse json empty lists" in {
    val json = parse("""
                       | {
                       |   "names": []
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(("CC", "case class CC(names: List[String])"))
  }

  it should "parse json with nested objects in arrays" in {
    val json =
      parse("""
        |{  
        |  "hotels": { 
        |    "entities": [
        |      { "id": "1aa4c4ad-f9ea-3367-a163-8a3a6884d450", "name": "Dana Beach Resort", "ids": [1,2,3] }
        |    ]
        |  }
        |}""".stripMargin)
        .fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(
      ("CC", "case class CC(hotels: hotels)"),
      ("hotels", "case class hotels(entities: List[entities])"),
      ("entities", "case class entities(name: String, ids: List[Double], id: String)")
    )
  }

  it should "parse json with different types in objects in arrays making them optional" in {
    val json = parse("""
                       | {
                       |   "list": [ { "id": 22, "other": 12 }, { "name": "Toben", "other": 12} ]
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(
      ("CC", "case class CC(list: List[list])"),
      ("list", "case class list(other: Double, name: Option[String], id: Option[Double])"))
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
      ("another", "case class another(double: double1)"),
      ("double1", "case class double1(xx: String)"),
      ("sub", "case class sub(double: double)"),
      ("double", "case class double(xx: String)"),
    )
  }

  it should "parse json with the same name but different structures" in {
    val json = parse("""
                       | {
                       |   "sub": { "double": { "xx": 22 } },
                       |   "another": { "double": { "xx": "sds" } }
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(
      ("CC", "case class CC(sub: sub, another: another)"),
      ("another", "case class another(double: double1)"),
      ("double1", "case class double1(xx: String)"),
      ("sub", "case class sub(double: double)"),
      ("double", "case class double(xx: Double)"),
    )
  }

  //todo
  ignore should "work for top level jsons" in {
    val json = parse("22").fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(
      ("CC", "Double")
    )
  }

  it should "work for different types in an array" in {
    val json = parse("""
                       | {
                       |   "list": [ 22, "Hi", false ]
                       | }
                     """.stripMargin).fold(throw _, x => x)

    jsonToCaseClass(json) shouldBe List(
      ("CC", "case class CC(list: List[Json])")
    )
  }
}
