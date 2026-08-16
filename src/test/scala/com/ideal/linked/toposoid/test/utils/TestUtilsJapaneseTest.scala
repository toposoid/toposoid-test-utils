/*
 * Copyright (C) 2025  Linked Ideal LLC.[https://linked-ideal.com/]
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.ideal.linked.toposoid.test.utils

import com.ideal.linked.common.DeploymentConverter.conf
import com.ideal.linked.toposoid.common.ToposoidUtils.assignId
import com.ideal.linked.toposoid.common.{FeatureType, SuperiorType, NonSentenceType, CaseGroupType, TransversalState, Neo4JUtilsImpl, ToposoidUtils}
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.{FeatureVectorIdentifier, FeatureVectorSearchResult, SingleFeatureVectorForSearch}
import com.ideal.linked.toposoid.knowledgebase.image.model.SingleImage
import com.ideal.linked.toposoid.knowledgebase.nlp.model.FeatureVector
import com.ideal.linked.toposoid.knowledgebase.regist.model.{ImageReference, Knowledge, KnowledgeForDocument, KnowledgeForImage, KnowledgeSentenceSet, PropositionRelation, Reference}
import com.ideal.linked.toposoid.protocol.model.neo4j.Neo4jRecords
import com.ideal.linked.toposoid.vectorizer.FeatureVectorizer
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.scalatest.flatspec.AnyFlatSpec
//import io.jvm.uuid.UUID
import play.api.libs.json.Json
import com.ideal.linked.toposoid.test.utils.TestUtils.getImageVector
import com.ideal.linked.toposoid.knowledgebase.regist.model.TableReference
import com.ideal.linked.toposoid.knowledgebase.regist.model.KnowledgeForTable
import com.ideal.linked.toposoid.test.utils.TestUtils.uploadTable
import com.ideal.linked.toposoid.test.utils.TestUtils.uploadImage

class TestUtilsJapaneseTest extends AnyFlatSpec with BeforeAndAfter with BeforeAndAfterAll {
  val transversalState: TransversalState = TransversalState(userId = "test-user", username = "guest", roleId = 0, csrfToken = "")
  val neo4JUtils = new Neo4JUtilsImpl()

  def deleteNeo4JAllData(transversalState: TransversalState): Unit = {
    val query = "MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r"
    neo4JUtils.executeQuery(query, transversalState)
  }

  before {
    ToposoidUtils.callComponent("{}", conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "createSchema", transversalState)
    ToposoidUtils.callComponent("{}", conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_PORT"), "createSchema", transversalState)
    ToposoidUtils.callComponent("{}", conf.getString("TOPOSOID_NON_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_NON_SENTENCE_VECTORDB_ACCESSOR_PORT"), "createSchema", transversalState)
    ToposoidUtils.callComponent("{}", conf.getString("TOPOSOID_TABLE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_TABLE_VECTORDB_ACCESSOR_PORT"), "createSchema", transversalState)
    deleteNeo4JAllData(transversalState)
    Thread.sleep(1000)
  }

  override def beforeAll(): Unit = {
    deleteNeo4JAllData(transversalState)
  }

  override def afterAll(): Unit = {
    deleteNeo4JAllData(transversalState)
  }

  /*
  private def deleteFeatureVector(featureVectorIdentifier: FeatureVectorIdentifier, featureType: FeatureType): Unit = {
    val json: String = Json.toJson(featureVectorIdentifier).toString()
    if (featureType.equals(SENTENCE)) {
      ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "delete", transversalState)
    } else if (featureType.equals(IMAGE)) {
      ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_PORT"), "delete", transversalState)
    }
  }
  
  private def getImageVector(url: String): FeatureVector = {
    val singleImage = SingleImage(url)
    val json: String = Json.toJson(singleImage).toString()
    val featureVectorJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_COMMON_IMAGE_RECOGNITION_HOST"), conf.getString("TOPOSOID_COMMON_IMAGE_RECOGNITION_PORT"), "getFeatureVector", transversalState)
    Json.parse(featureVectorJson).as[FeatureVector]
  }
  */

  "The data " should "be properly registered in GraphDB and VectorDB." in {
    val documentId = java.util.UUID.randomUUID().toString
    val knowledge1 = Knowledge(sentence = "これはテストの前提1です。", lang = "ja_JP", extentInfoJson = "{}")
    val knowledge2 = Knowledge(sentence = "これはテストの前提2です。", lang = "ja_JP", extentInfoJson = "{}")
    val reference3 = Reference(url = "", surface = "猫が", surfaceIndex = 0, isWholeSentence = false, originalUrlOrReference = "http://images.cocodataset.org/val2017/000000039769.jpg", metaInformations = List.empty[String])
    val imageReference3 = ImageReference(reference = reference3, x = 27, y = 41, width = 287, height = 435)
    val knowledgeForImages3 = uploadImage(KnowledgeForImage(id = "", imageReference = imageReference3), transversalState)
    val knowledge3 = Knowledge(sentence = "猫が２匹います。", lang = "ja_JP", extentInfoJson = "{}", knowledgeForImages = List(knowledgeForImages3))
    
    val reference3a= Reference(url = "", surface = "データが", surfaceIndex = 0, isWholeSentence = false, originalUrlOrReference = "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000001086170&fileKind=0", metaInformations = List.empty[String])
    val tableReference3a = TableReference(reference = reference3a, skipHeaderRows= 5, multiHeaderRowsForExcel=4, sheetNameForExcel="se0101")
    val knowledgeForTable3a = uploadTable(KnowledgeForTable(id = "", tableReference = tableReference3a), transversalState)
    val knowledge3a = Knowledge(sentence = "データがあります。", lang = "ja_JP", extentInfoJson = "{}", knowledgeForTables=List(knowledgeForTable3a))

    val knowledgeForDocument = KnowledgeForDocument(id = documentId, filename = "Test.pdf", url = "http://example.com/Test.pdf", titleOfTopPage = "テストタイトル")

    val knowledge4 = Knowledge(sentence = "これはテストの主張1です。", lang = "ja_JP", extentInfoJson = "{}", knowledgeForDocument=knowledgeForDocument)
    val knowledge5 = Knowledge(sentence = "これはテストの主張2です。", lang = "ja_JP", extentInfoJson = "{}")
    val reference6 = Reference(url = "", surface = "犬が", surfaceIndex = 0, isWholeSentence = false, originalUrlOrReference = "http://images.cocodataset.org/train2017/000000428746.jpg", metaInformations = List.empty[String])
    val imageReference6 = ImageReference(reference = reference6, x = 435, y = 227, width = 91, height = 69)
    val knowledgeForImages6 = uploadImage(KnowledgeForImage(id = "", imageReference = imageReference6),transversalState)
    val knowledge6 = Knowledge(sentence = "犬が1匹います。", lang = "ja_JP", extentInfoJson = "{}", knowledgeForImages = List(knowledgeForImages6))

    val reference7 = Reference(url = "", surface = "証拠が", surfaceIndex = 0, isWholeSentence = false, originalUrlOrReference = "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000001086171&fileKind=0", metaInformations = List.empty[String])
    val tableReference7 = TableReference(reference = reference7, skipHeaderRows= 8, multiHeaderRowsForExcel=4, sheetNameForExcel="se0102")
    val knowledgeForTable7 = uploadTable(KnowledgeForTable(id = "", tableReference = tableReference7), transversalState)
    val knowledge7 = Knowledge(sentence = "証拠があります。", lang = "ja_JP", extentInfoJson = "{}", knowledgeForTables = List(knowledgeForTable7))

    val knowledgeSentenceSet: KnowledgeSentenceSet = KnowledgeSentenceSet(
      premiseList = List(knowledge1, knowledge2, knowledge3, knowledge3a),
      premiseLogicRelation = List(PropositionRelation(operator = "AND", sourceIndex = 0, destinationIndex = 1), PropositionRelation(operator = "AND", sourceIndex = 0, destinationIndex = 2), PropositionRelation(operator = "AND", sourceIndex = 0, destinationIndex = 3)),
      claimList = List(knowledge4, knowledge5, knowledge6, knowledge7),
      claimLogicRelation = List(PropositionRelation(operator = "OR", sourceIndex = 0, destinationIndex = 1), PropositionRelation(operator = "AND", sourceIndex = 0, destinationIndex = 2), PropositionRelation(operator = "AND", sourceIndex = 0, destinationIndex = 3))
    )
    val (knowledgeSentenceSetForParser, propositionId) = assignId(knowledgeSentenceSet)
    TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)

    val query = "MATCH x=(:ClaimNode{surface:'主張２です。'})<-[:LocalEdge{logicType:'OR'}]-(:ClaimNode{surface:'主張１です。'})<-[:LocalEdge{logicType:'IMP'}]-(:PremiseNode{surface:'前提１です。'})-[:LocalEdge{logicType:'AND'}]->(:PremiseNode{surface:'前提２です。'}) return x"
    val queryResult: Neo4jRecords = neo4JUtils.executeQueryAndReturn(query, transversalState)
    assert(queryResult.records.size == 1)
    val result2: Neo4jRecords = neo4JUtils.executeQueryAndReturn("MATCH (s:ImageNode{source:'http://images.cocodataset.org/val2017/000000039769.jpg'})-[:ImageEdge]->(t:PremiseNode{surface:'猫が'}) RETURN s, t", transversalState)
    assert(result2.records.size == 1)
    val urlCat = result2.records.head.head.value.featureNode.get.url
    val result3: Neo4jRecords = neo4JUtils.executeQueryAndReturn("MATCH (s:ImageNode{source:'http://images.cocodataset.org/train2017/000000428746.jpg'})-[:ImageEdge]->(t:ClaimNode{surface:'犬が'}) RETURN s, t", transversalState)
    assert(result3.records.size == 1)
    val urlDog = result3.records.head.head.value.featureNode.get.url
    val result4: Neo4jRecords = neo4JUtils.executeQueryAndReturn("MATCH x = (:GlobalNode{titleOfTopPage:'テストタイトル'}) RETURN x", transversalState)
    assert(result4.records.size == 1)

    val result5: Neo4jRecords = neo4JUtils.executeQueryAndReturn("MATCH (s:TableNode{source:'https://www.e-stat.go.jp/stat-search/file-download?statInfId=000001086170&fileKind=0'})-[:TableEdge]->(t:PremiseNode{surface:'データが'}) RETURN s, t", transversalState)
    val urlTable1 = result5.records.head.head.value.featureNode.get.url
    assert(result5.records.size == 1)
    val result6: Neo4jRecords = neo4JUtils.executeQueryAndReturn("MATCH (s:TableNode{source:'https://www.e-stat.go.jp/stat-search/file-download?statInfId=000001086171&fileKind=0'})-[:TableEdge]->(t:ClaimNode{surface:'証拠が'}) RETURN s, t", transversalState)
    val urlTable2 = result6.records.head.head.value.featureNode.get.url
    assert(result6.records.size == 1)

    for (knowledge <- knowledgeSentenceSet.premiseList ::: knowledgeSentenceSet.claimList) {
      val vector = FeatureVectorizer.getSentenceVector(Knowledge(knowledge.sentence, "ja_JP", "{}"), transversalState)
      val json: String = Json.toJson(SingleFeatureVectorForSearch(vector = vector.vector, num = 1)).toString()
      val featureVectorSearchResultJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "search", transversalState)
      val result = Json.parse(featureVectorSearchResultJson).as[FeatureVectorSearchResult]
      assert(result.ids.size > 0 && result.similarities.head > 0.999)
      //result.ids.map(x => deleteFeatureVector(x, SENTENCE))

      knowledge.knowledgeForImages.foreach(x => {
        val url: String = x.imageReference.reference.surface match {
          case "猫が" => urlCat
          case "犬が" => urlDog
          case _ => "BAD URL"
        }
        val vector = getImageVector(url, transversalState)
        val json: String = Json.toJson(SingleFeatureVectorForSearch(vector = vector.vector, num = 1)).toString()
        val featureVectorSearchResultJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_PORT"), "search", transversalState)
        val result = Json.parse(featureVectorSearchResultJson).as[FeatureVectorSearchResult]
        assert(result.ids.size > 0 && result.similarities.head > 0.999)
        //result.ids.map(x => deleteFeatureVector(x, IMAGE))
      })

      knowledge.knowledgeForTables.foreach(x => {
        val url: String = x.tableReference.reference.surface match {
          case "データが" => urlTable1
          case "証拠が" => urlTable2
          case _ => "BAD URL"
        }
        val vector = TestUtils.getTableVector(url, transversalState)
        val json: String = Json.toJson(SingleFeatureVectorForSearch(vector = vector.vector, num = 1)).toString()
        val featureVectorSearchResultJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_TABLE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_TABLE_VECTORDB_ACCESSOR_PORT"), "search", transversalState)
        val result = Json.parse(featureVectorSearchResultJson).as[FeatureVectorSearchResult]
        assert(result.ids.size > 0 && result.similarities.filter(x => x > 0.95).size > 0)
        //result.ids.map(x => TestUtils.deleteFeatureVector(x, FeatureType.TABLE, transversalState))
      })

      if(!knowledge.knowledgeForDocument.id.equals("")){
        val vector = FeatureVectorizer.getSentenceVector(Knowledge(knowledge.knowledgeForDocument.titleOfTopPage, "ja_JP", "{}"), transversalState)
        val json: String = Json.toJson(SingleFeatureVectorForSearch(vector = vector.vector, num = 1)).toString()
        val featureVectorSearchResultJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_NON_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_NON_SENTENCE_VECTORDB_ACCESSOR_PORT"), "search", transversalState)
        val result = Json.parse(featureVectorSearchResultJson).as[FeatureVectorSearchResult]
        assert(result.ids.size > 0 && result.similarities.head > 0.999)
        //result.ids.map(x => deleteFeatureVector(x, SENTENCE))
      }

    }
    TestUtils.deleteData(knowledgeSentenceSetForParser, transversalState)

    neo4JUtils.executeQuery("MATCH (n) RETURN n", transversalState)
    val check1: Neo4jRecords = neo4JUtils.executeQueryAndReturn(query, transversalState)
    assert(check1.records.size == 0)

    val featureVectorIdentifierSV = FeatureVectorIdentifier(propositionId, "-", -1, "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index)
    val jsonSV: String = Json.toJson(featureVectorIdentifierSV).toString()
    val featureVectorSearchResultJsonSV: String = ToposoidUtils.callComponent(jsonSV, conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "searchBySuperiorId", transversalState)
    val checkSV = Json.parse(featureVectorSearchResultJsonSV).as[FeatureVectorSearchResult]
    assert(checkSV.ids.size == 0)

    val featureVectorIdentifierIMGV = FeatureVectorIdentifier(propositionId, "-", -1, "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index)
    val jsonIMGV: String = Json.toJson(featureVectorIdentifierIMGV).toString()
    val featureVectorSearchResultJsonIMGV: String = ToposoidUtils.callComponent(jsonIMGV, conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_PORT"), "searchBySuperiorId", transversalState)
    val checkIMGV = Json.parse(featureVectorSearchResultJsonIMGV).as[FeatureVectorSearchResult]
    assert(checkIMGV.ids.size == 0)

    val featureVectorIdentifierNSV = FeatureVectorIdentifier(documentId, "-", -1, "ja_JP", SuperiorType.DOCUMENT_ID.index, NonSentenceType.TITLE_OF_TOP_PAGE.index, CaseGroupType.UNSPECIFIED.index)
    val jsonNSV: String = Json.toJson(featureVectorIdentifierNSV).toString()
    val featureVectorSearchResultJsonNSV: String = ToposoidUtils.callComponent(jsonNSV, conf.getString("TOPOSOID_NON_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_NON_SENTENCE_VECTORDB_ACCESSOR_PORT"), "searchBySuperiorId", transversalState)
    val resultNSV = Json.parse(featureVectorSearchResultJsonNSV).as[FeatureVectorSearchResult]
    assert(resultNSV.ids.size == 0)

    val featureVectorIdentifierTABLV = FeatureVectorIdentifier(propositionId, "-", -1, "ja_JP", SuperiorType.PROPOSITION_ID.index, NonSentenceType.UNSPECIFIED.index, CaseGroupType.UNSPECIFIED.index)
    val jsonTABLV: String = Json.toJson(featureVectorIdentifierTABLV).toString()
    val featureVectorSearchResultJsonTABLV: String = ToposoidUtils.callComponent(jsonTABLV, conf.getString("TOPOSOID_TABLE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_TABLE_VECTORDB_ACCESSOR_PORT"), "searchBySuperiorId", transversalState)
    val resultTABLV = Json.parse(featureVectorSearchResultJsonTABLV).as[FeatureVectorSearchResult]
    assert(resultTABLV.ids.size == 0)


  }


}






