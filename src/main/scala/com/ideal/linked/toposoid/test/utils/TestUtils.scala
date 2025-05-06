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
import com.ideal.linked.toposoid.common.{ToposoidUtils, TransversalState}
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.RegistContentResult
import com.ideal.linked.toposoid.knowledgebase.regist.model.{Knowledge, KnowledgeForImage}
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObjects
import com.ideal.linked.toposoid.protocol.model.neo4j.Neo4jRecords
import com.ideal.linked.toposoid.protocol.model.parser.{InputSentenceForParser, KnowledgeForParser, KnowledgeSentenceSetForParser}
import com.ideal.linked.toposoid.sentence.transformer.neo4j.{AnalyzedPropositionPair, AnalyzedPropositionSet, Neo4JUtilsImpl, Sentence2Neo4jTransformer}
import com.ideal.linked.toposoid.vectorizer.FeatureVectorizer
import play.api.libs.json.Json
import io.jvm.uuid.UUID
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object TestUtils extends App {


  private def parse(knowledgeForParser: KnowledgeForParser, transversalState: TransversalState): AnalyzedPropositionPair = {

    val langPatternJP: Regex = "^ja_.*".r
    val langPatternEN: Regex = "^en_.*".r

    //Analyze everything as simple sentences as Claims, not just sentenceType
    val inputSentenceForParser = InputSentenceForParser(List.empty[KnowledgeForParser], List(knowledgeForParser))
    val json: String = Json.toJson(inputSentenceForParser).toString()
    val parserInfo: (String, String) = knowledgeForParser.knowledge.lang match {
      case langPatternJP() => (conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"))
      case langPatternEN() => (conf.getString("TOPOSOID_SENTENCE_PARSER_EN_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_EN_WEB_PORT"))
      case _ => throw new Exception("It is an invalid locale or an unsupported locale.")
    }
    val parseResult: String = ToposoidUtils.callComponent(json, parserInfo._1, parserInfo._2, "analyze", transversalState)
    val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(parseResult).as[AnalyzedSentenceObjects]
    AnalyzedPropositionPair(analyzedSentenceObjects = analyzedSentenceObjects, knowledgeForParser = knowledgeForParser)
  }

  private def getAnalyzedPropositionSet(knowledgeSentenceSetForParser: KnowledgeSentenceSetForParser, transversalState: TransversalState): AnalyzedPropositionSet = {

    val premiseList = knowledgeSentenceSetForParser.premiseList.size match  {
      case 0 => List.empty[AnalyzedPropositionPair]
      case _ => knowledgeSentenceSetForParser.premiseList.map(parse(_, transversalState))
    }
    val claimList = knowledgeSentenceSetForParser.claimList.map(parse(_, transversalState))
    AnalyzedPropositionSet(
      premiseList = premiseList,
      premiseLogicRelation = knowledgeSentenceSetForParser.premiseLogicRelation,
      claimList = claimList,
      claimLogicRelation = knowledgeSentenceSetForParser.claimLogicRelation)
  }

  private def registKnowledgeImages(knowledgeForParsers: List[KnowledgeForParser], transversalState: TransversalState): List[KnowledgeForParser] = Try {

    knowledgeForParsers.foldLeft(List.empty[KnowledgeForParser]) {
      (acc, x) => {
        val knowledgeForImages: List[KnowledgeForImage] = x.knowledge.knowledgeForImages.map(y => {
          val imageFeatureId = UUID.random.toString
          val json: String = Json.toJson(KnowledgeForImage(imageFeatureId, y.imageReference)).toString()
          val knowledgeForImageJson: String = ToposoidUtils.callComponent(json,
            conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
            conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
            "registImage", transversalState)
          val registContentResult: RegistContentResult = Json.parse(knowledgeForImageJson).as[RegistContentResult]
          if (registContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registContentResult.statusInfo.message)
          registContentResult.knowledgeForImage
        })
        val knowledge = Knowledge(sentence = x.knowledge.sentence,
          lang = x.knowledge.lang, extentInfoJson = x.knowledge.extentInfoJson,
          isNegativeSentence = x.knowledge.isNegativeSentence, knowledgeForImages)
        acc :+ KnowledgeForParser(x.propositionId, x.sentenceId, knowledge)
      }
    }
  } match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  def registerData(knowledgeSentenceSetForParser: KnowledgeSentenceSetForParser, transversalState: TransversalState, addVectorFlag: Boolean = true): Unit = {

    val knowledgeSentenceSetForParserWithImage = KnowledgeSentenceSetForParser(
      registKnowledgeImages(knowledgeSentenceSetForParser.premiseList, transversalState),
      knowledgeSentenceSetForParser.premiseLogicRelation,
      registKnowledgeImages(knowledgeSentenceSetForParser.claimList, transversalState),
      knowledgeSentenceSetForParser.claimLogicRelation)

    val analyzedPropositionSet = getAnalyzedPropositionSet(knowledgeSentenceSetForParserWithImage, transversalState)
    Sentence2Neo4jTransformer.createGraph(analyzedPropositionSet, transversalState)
    if (addVectorFlag) FeatureVectorizer.createVector(knowledgeSentenceSetForParserWithImage, transversalState)
  }

  def executeQueryAndReturn(query: String, transversalState: TransversalState): Neo4jRecords = {
    val convertQuery = ToposoidUtils.encodeJsonInJson(query)
    val hoge = ToposoidUtils.decodeJsonInJson(convertQuery)
    val json = s"""{ "query":"$convertQuery", "target": "" }"""
    val jsonResult = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_GRAPHDB_WEB_HOST"), conf.getString("TOPOSOID_GRAPHDB_WEB_PORT"), "getQueryFormattedResult", transversalState)
    Json.parse(jsonResult).as[Neo4jRecords]
  }

  def deleteNeo4JAllData(transversalState: TransversalState): Unit = {
    val query = "MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r"
    val neo4JUtils = new Neo4JUtilsImpl()
    neo4JUtils.executeQuery(query, transversalState)
  }


  /*
  private def deleteObject(knowledgeForParser: KnowledgeForParser, transversalState: TransversalState) = {
    //TODO:documentIdを持っているノードも削除
    //Delete relationships
    val query = s"MATCH (n)-[r]-() WHERE n.propositionId = '${knowledgeForParser.propositionId}' DELETE n,r"
    val neo4JUtils = new Neo4JUtilsImpl()
    neo4JUtils.executeQuery(query, transversalState)
    //Delete orphan nodes
    val query2 = s"MATCH (n) WHERE n.propositionId = '${knowledgeForParser.propositionId}' DELETE n"
    neo4JUtils.executeQuery(query2, transversalState)
    val query3 = s"MATCH (n) WHERE n.documentId = '${knowledgeForParser.knowledge.knowledgeForDocument.id}' DELETE n"
    neo4JUtils.executeQuery(query3, transversalState)
    FeatureVectorizer.removeVector(knowledgeForParser, transversalState)
  }

  def deleteFeatureVector(featureVectorIdentifier: FeatureVectorIdentifier, featureType: FeatureType, transversalState: TransversalState): Unit = {
    val json: String = Json.toJson(featureVectorIdentifier).toString()
    if (featureType.equals(SENTENCE)) {
      ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "delete", transversalState)
    } else if (featureType.equals(IMAGE)) {
      ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_PORT"), "delete", transversalState)
    } else if (featureType.equals(NON_SENTENCE)){
      ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_NON_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_NON_SENTENCE_VECTORDB_ACCESSOR_PORT"), "delete", transversalState)
    }
  }
  */



}
