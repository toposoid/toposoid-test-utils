package com.ideal.linked.toposoid.test.utils

import com.ideal.linked.common.DeploymentConverter.conf
import com.ideal.linked.toposoid.common._
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.{FeatureVectorIdentifier, FeatureVectorSearchResult, SingleFeatureVectorForSearch}
import com.ideal.linked.toposoid.knowledgebase.image.model.SingleImage
import com.ideal.linked.toposoid.knowledgebase.nlp.model.FeatureVector
import com.ideal.linked.toposoid.knowledgebase.regist.model._
import com.ideal.linked.toposoid.protocol.model.neo4j.Neo4jRecords
import com.ideal.linked.toposoid.protocol.model.parser.{KnowledgeForParser, KnowledgeSentenceSetForParser}
import com.ideal.linked.toposoid.vectorizer.FeatureVectorizer
import io.jvm.uuid.UUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import play.api.libs.json.Json

class TestUtilsEnglishTest extends AnyFlatSpec with BeforeAndAfter with BeforeAndAfterAll {
  val transversalState: TransversalState = TransversalState(userId = "test-user", username = "guest", roleId = 0, csrfToken = "")
  before {
    ToposoidUtils.callComponent("{}", conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "createSchema", transversalState)
    ToposoidUtils.callComponent("{}", conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_PORT"), "createSchema", transversalState)
    TestUtils.deleteNeo4JAllData(transversalState)
    Thread.sleep(1000)
  }

  override def beforeAll(): Unit = {
    TestUtils.deleteNeo4JAllData(transversalState)
  }

  override def afterAll(): Unit = {
    TestUtils.deleteNeo4JAllData(transversalState)
  }

  private def convertKnowledge(knowledge: Knowledge): Knowledge = {
    val knowledgeForImages: List[KnowledgeForImage] = knowledge.knowledgeForImages.map(y => {
      val imageFeatureId = UUID.random.toString
      KnowledgeForImage(imageFeatureId, y.imageReference)
    })
    Knowledge(knowledge.sentence, knowledge.lang, knowledge.extentInfoJson, knowledge.isNegativeSentence, knowledgeForImages)
  }

  def assignId(knowledgeSentenceSet: KnowledgeSentenceSet): (KnowledgeSentenceSetForParser, String) = {
    val propositionId = UUID.random.toString
    val knowledgeForParserPremise: List[KnowledgeForParser] = knowledgeSentenceSet.premiseList.map(x => KnowledgeForParser(propositionId, UUID.random.toString, convertKnowledge(x)))
    val knowledgeForParserClaim: List[KnowledgeForParser] = knowledgeSentenceSet.claimList.map(x => KnowledgeForParser(propositionId, UUID.random.toString, convertKnowledge(x)))

    (KnowledgeSentenceSetForParser(
      premiseList = knowledgeForParserPremise,
      premiseLogicRelation = knowledgeSentenceSet.premiseLogicRelation,
      claimList = knowledgeForParserClaim,
      claimLogicRelation = knowledgeSentenceSet.claimLogicRelation
    ), propositionId)
  }

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

  "The data " should "be properly registered in GraphDB and VectorDB." in {

    val knowledge1 = Knowledge(sentence = "This is premise-1.", lang = "en_US", extentInfoJson = "{}")
    val knowledge2 = Knowledge(sentence = "This is premise-2.", lang = "en_US", extentInfoJson = "{}")
    val reference3 = Reference(url = "", surface = "cats", surfaceIndex = 3, isWholeSentence = false, originalUrlOrReference = "http://images.cocodataset.org/val2017/000000039769.jpg", metaInformations = List.empty[String])
    val imageReference3 = ImageReference(reference = reference3, x = 27, y = 41, width = 287, height = 435)
    val knowledgeForImages3 = KnowledgeForImage(id = "", imageReference = imageReference3)
    val knowledge3 = Knowledge(sentence = "There are two cats.", lang = "en_US", extentInfoJson = "{}", knowledgeForImages = List(knowledgeForImages3))

    val knowledge4 = Knowledge(sentence = "This is claim-1.", lang = "en_US", extentInfoJson = "{}")
    val knowledge5 = Knowledge(sentence = "This is claim-2.", lang = "en_US", extentInfoJson = "{}")
    val reference6 = Reference(url = "", surface = "dog", surfaceIndex = 3, isWholeSentence = false, originalUrlOrReference = "http://images.cocodataset.org/train2017/000000428746.jpg", metaInformations = List.empty[String])
    val imageReference6 = ImageReference(reference = reference6, x = 435, y = 227, width = 91, height = 69)
    val knowledgeForImages6 = KnowledgeForImage(id = "", imageReference = imageReference6)
    val knowledge6 = Knowledge(sentence = "There is a dog", lang = "en_US", extentInfoJson = "{}", knowledgeForImages = List(knowledgeForImages6))

    val knowledgeSentenceSet = KnowledgeSentenceSet(
      premiseList = List(knowledge1, knowledge2, knowledge3),
      premiseLogicRelation = List(PropositionRelation(operator = "AND", sourceIndex = 0, destinationIndex = 1), PropositionRelation(operator = "AND", sourceIndex = 0, destinationIndex = 2)),
      claimList = List(knowledge4, knowledge5, knowledge6),
      claimLogicRelation = List(PropositionRelation(operator = "OR", sourceIndex = 0, destinationIndex = 1), PropositionRelation(operator = "AND", sourceIndex = 0, destinationIndex = 2))
    )
    val (knowledgeSentenceSetForParser, propositionId) = assignId(knowledgeSentenceSet)
    TestUtils.registerData(knowledgeSentenceSetForParser, transversalState)

    val query = "MATCH x=(:ClaimNode{surface:'claim-1'})-[:LocalEdge]-(:ClaimNode)-[:LocalEdge{logicType:'OR'}]-(:ClaimNode)-[:LocalEdge]-(:ClaimNode{surface:'claim-2'}) return x"
    val queryResult: Neo4jRecords = TestUtils.executeQueryAndReturn(query, transversalState)
    assert(queryResult.records.size == 1)
    val query2 = "MATCH x=(:PremiseNode{surface:'premise-1'})-[:LocalEdge]-(:PremiseNode)-[:LocalEdge{logicType:'AND'}]-(:PremiseNode)-[:LocalEdge]-(:PremiseNode{surface:'premise-2'}) return x"
    val queryResult2: Neo4jRecords = TestUtils.executeQueryAndReturn(query2, transversalState)
    assert(queryResult2.records.size == 1)
    val query3 = "MATCH x=(:PremiseNode{surface:'premise-1'})-[:LocalEdge]-(:PremiseNode)-[:LocalEdge{logicType:'IMP'}]-(:ClaimNode)-[:LocalEdge]-(:ClaimNode{surface:'claim-1'}) return x"
    val queryResult3: Neo4jRecords = TestUtils.executeQueryAndReturn(query3, transversalState)
    assert(queryResult3.records.size == 1)


    val queryResult4: Neo4jRecords = TestUtils.executeQueryAndReturn("MATCH (s:ImageNode{source:'http://images.cocodataset.org/val2017/000000039769.jpg'})-[:ImageEdge]->(t:PremiseNode{surface:'cats'}) RETURN s, t", transversalState)
    assert(queryResult4.records.size == 1)

    val urlCat = queryResult4.records.head.head.value.featureNode.get.url
    val queryResult5: Neo4jRecords = TestUtils.executeQueryAndReturn("MATCH (s:ImageNode{source:'http://images.cocodataset.org/train2017/000000428746.jpg'})-[:ImageEdge]->(t:ClaimNode{surface:'dog'}) RETURN s, t", transversalState)
    assert(queryResult5.records.size == 1)
    val urlDog = queryResult5.records.head.head.value.featureNode.get.url

    for (knowledge <- knowledgeSentenceSet.premiseList ::: knowledgeSentenceSet.claimList) {
      val vector = FeatureVectorizer.getSentenceVector(Knowledge(knowledge.sentence, "en_US", "{}"), transversalState)
      val json: String = Json.toJson(SingleFeatureVectorForSearch(vector = vector.vector, num = 1)).toString()
      val featureVectorSearchResultJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "search", transversalState)
      val result = Json.parse(featureVectorSearchResultJson).as[FeatureVectorSearchResult]
      assert(result.ids.size > 0)
      result.ids.map(x => deleteFeatureVector(x, SENTENCE))

      knowledge.knowledgeForImages.foreach(x => {
        val url: String = x.imageReference.reference.surface match {
          case "cats" => urlCat
          case "dog" => urlDog
          case _ => "BAD URL"
        }
        val vector = this.getImageVector(url)
        val json: String = Json.toJson(SingleFeatureVectorForSearch(vector = vector.vector, num = 1)).toString()
        val featureVectorSearchResultJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_PORT"), "search", transversalState)
        val result = Json.parse(featureVectorSearchResultJson).as[FeatureVectorSearchResult]
        assert(result.ids.size > 0 && result.similarities.head > 0.999)
        result.ids.map(x => deleteFeatureVector(x, IMAGE))
      })

    }

  }


}






