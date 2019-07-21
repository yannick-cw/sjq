package io.yannick_cw.sjq

import io.circe.parser.parse
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import io.circe._
import io.circe.testing.instances._

class ExecuteAccessPatternTest extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  it should "work for complex json" in {
    (for {
      res       <- executeAccessPattern("root", complexJson)
      expected  <- parse(complexJson)
      parsedRes <- parse(res)
    } yield parsedRes shouldBe expected).fold(throw _, x => x)
  }

  ignore should "work for all json" in {
    forAll { json: Json =>
      whenever(json.isObject) {
        (for {
          res <- executeAccessPattern("root", json.spaces2)
          expected = json
          parsedRes <- parse(res)
        } yield parsedRes shouldBe expected).fold(throw _, x => x)
      }
    }
  }

  val complexJson = """
      |{
      |"query": "mall",
      |"hotels": {
      |"count": 9305,
      |"position": 2,
      |"entities": [
      |{
      |"id": "bcbd353a-5e08-4ffc-b84a-86aac88568ef",
      |"name": "Close to Wiregrass mall & outlet mall",
      |"classifier": "HOTEL",
      |"highlighted": "Close to Wiregrass <strong>mall</strong> & outlet <strong>mall</strong>",
      |"score": 660.220703125,
      |"parents": [
      |"Wesley Chapel",
      |"Florida",
      |"USA"
      |],
      |"holidayRegions": [
      |"Florida Westküste",
      |"Golf von Mexiko",
      |"Florida Küste",
      |"USA Ostküste"
      |],
      |"rankingScore": 1,
      |"recommendationRate": 0,
      |"alternativeNames": [],
      |"placeDetailString": "Hotel in Wesley Chapel, Florida, USA",
      |"highlightedName": "Close to Wiregrass <strong>mall</strong> & outlet <strong>mall</strong>",
      |"highlightedParents": [],
      |"highlightedPlaceDetailString": ""
      |}
      |]
      |},
      |"destinations": {
      |"count": 33,
      |"position": 1,
      |"entities": [
      |{
      |"id": "07f5f656-4acc-3230-b7dd-aec3c13af37c",
      |"name": "Mallorca",
      |"classifier": "DESTINATION_REGION",
      |"highlighted": "<strong>Mall</strong>orca",
      |"score": 1009.79931640625,
      |"parents": [
      |"Spanien"
      |],
      |"holidayRegions": [],
      |"rankingScore": 526309,
      |"recommendationRate": null,
      |"alternativeNames": [],
      |"placeDetailString": "Region in Spanien",
      |"highlightedName": "<strong>Mall</strong>orca",
      |"highlightedParents": [],
      |"highlightedPlaceDetailString": ""
      |},
      |{
      |"id": "67fb4605-9516-3cf9-8d02-53d87086c57a",
      |"name": "Malles Venosta / Mals",
      |"classifier": "DESTINATION_CITY",
      |"highlighted": "<strong>Mals</strong>",
      |"score": 495.9881896972656,
      |"parents": [
      |"Südtirol",
      |"Italien"
      |],
      |"holidayRegions": [
      |"Alpen",
      |"Skigebiet Reschenpass in Südtirol",
      |"Vinschgau"
      |],
      |"rankingScore": 584,
      |"recommendationRate": null,
      |"alternativeNames": [],
      |"placeDetailString": "Ort in Südtirol, Italien",
      |"highlightedName": "<strong>Mals</strong>",
      |"highlightedParents": [],
      |"highlightedPlaceDetailString": ""
      |},
      |{
      |"id": "46804445-c792-340b-beb1-003b37b37c36",
      |"name": "Mallow",
      |"classifier": "DESTINATION_CITY",
      |"highlighted": "<strong>Mall</strong>ow",
      |"score": 433.5486145019531,
      |"parents": [
      |"Munster",
      |"Irland"
      |],
      |"holidayRegions": [],
      |"rankingScore": 284,
      |"recommendationRate": null,
      |"alternativeNames": [],
      |"placeDetailString": "Ort in Munster, Irland",
      |"highlightedName": "<strong>Mall</strong>ow",
      |"highlightedParents": [],
      |"highlightedPlaceDetailString": ""
      |},
      |{
      |"id": "523f034a-064e-3251-805a-55eca882ce68",
      |"name": "Mallnitz",
      |"classifier": "DESTINATION_CITY",
      |"highlighted": "<strong>Mall</strong>nitz",
      |"score": 360.3058166503906,
      |"parents": [
      |"Kärnten",
      |"Österreich"
      |],
      |"holidayRegions": [
      |"Hohe Tauern",
      |"Skigebiet Mölltaler Gletscher in Kärnten",
      |"Alpen",
      |"Pongau",
      |"Skigebiet Großarltal - Dorfgastein (Ski Amadé)"
      |],
      |"rankingScore": 108,
      |"recommendationRate": null,
      |"alternativeNames": [],
      |"placeDetailString": "Ort in Kärnten, Österreich",
      |"highlightedName": "<strong>Mall</strong>nitz",
      |"highlightedParents": [],
      |"highlightedPlaceDetailString": ""
      |},
      |{
      |"id": "b934be2d-e042-3a8e-8fcc-00301dc2fb7d",
      |"name": "Palma de Mallorca",
      |"classifier": "DESTINATION_CITY",
      |"highlighted": "Palma de <strong>Mall</strong>orca",
      |"score": 357.1685791015625,
      |"parents": [
      |"Mallorca",
      |"Spanien"
      |],
      |"holidayRegions": [
      |"Balearen"
      |],
      |"rankingScore": 46650,
      |"recommendationRate": null,
      |"alternativeNames": [],
      |"placeDetailString": "Ort in Mallorca, Spanien",
      |"highlightedName": "Palma de <strong>Mall</strong>orca",
      |"highlightedParents": [
      |"<strong>Mall</strong>orca"
      |],
      |"highlightedPlaceDetailString": "Ort in <strong>Mall</strong>orca, Spanien"
      |}
      |]
      |},
      |"bucketPriority": {
      |"hotelProbability": 0.00026465528648934763,
      |"destinationProbability": 0.9997353447135107
      |},
      |"passions": [],
      |"cruises": []
      |}
    """.stripMargin
}
