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
import com.ideal.linked.toposoid.common.{Neo4JUtils, Neo4JUtilsImpl, ToposoidUtils, TransversalState, ActionModeType}
import com.ideal.linked.toposoid.knowledgebase.regist.model.{Knowledge, KnowledgeForImage, KnowledgeForTable}
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObjects
import com.ideal.linked.toposoid.protocol.model.parser.{InputSentenceForParser, KnowledgeForParser, KnowledgeSentenceSetForParser}
import com.ideal.linked.toposoid.sentence.transformer.neo4j.{AnalyzedPropositionPair, AnalyzedPropositionSet, Sentence2Neo4jTransformer}
import com.ideal.linked.toposoid.vectorizer.FeatureVectorizer
import play.api.libs.json.Json

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex
import com.ideal.linked.toposoid.protocol.model.base.VerifyingEdges
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObject
import com.ideal.linked.toposoid.protocol.model.base.DeductionResult
import com.ideal.linked.toposoid.protocol.model.frontend.Endpoint
import com.ideal.linked.toposoid.common.InMemoryDbUtils
import com.ideal.linked.toposoid.common.DeductionPhaseType
import java.net.URI
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.FeatureVectorIdentifier
import com.ideal.linked.toposoid.common.FeatureType
import com.ideal.linked.toposoid.knowledgebase.table.model.SingleTable
import com.ideal.linked.toposoid.knowledgebase.nlp.model.FeatureVector
import com.ideal.linked.toposoid.knowledgebase.image.model.SingleImage
import sttp.client4.DefaultSyncBackend
import sttp.client4.BackendOptions
import sttp.client4.basicRequest
import sttp.client4.UriContext
import sttp.client4.multipart
import sttp.client4.multipartFile
import java.nio.file.Paths
import scala.concurrent.duration.{Duration, DurationInt}
import sttp.model.HttpVersion
import com.ideal.linked.toposoid.common.TRANSVERSAL_STATE
import java.nio.file.Path
import com.ideal.linked.toposoid.knowledgebase.regist.model.Reference
import com.ideal.linked.toposoid.knowledgebase.regist.model.ImageReference
import play.api.libs.json.{Json, OWrites, Reads}
import com.ideal.linked.toposoid.knowledgebase.image.model.RegisteredImageContentResult
import com.ideal.linked.toposoid.knowledgebase.table.model.RegisteredTableContentResult
import com.ideal.linked.toposoid.knowledgebase.regist.model.TableReference
import com.ideal.linked.toposoid.knowledgebase.model.LocalContextForFeature
import com.ideal.linked.toposoid.knowledgebase.model.KnowledgeFeatureReference
import com.ideal.linked.toposoid.knowledgebase.model.KnowledgeBaseSemiGlobalNode
import com.ideal.linked.toposoid.common.DataEntryType
import com.ideal.linked.toposoid.knowledgebase.model.KnowledgeBaseNode
import com.ideal.linked.toposoid.knowledgebase.model.LocalContext

case class UploadResult(id: String, url:String, status:Int)
object UploadResult {
  implicit val jsonWrites: OWrites[UploadResult] = Json.writes[UploadResult]
  implicit val jsonReads: Reads[UploadResult] = Json.reads[UploadResult]
}

object TestUtils {

  private def parse(knowledgeForParser: KnowledgeForParser, transversalState: TransversalState): AnalyzedPropositionPair = {

    val langPatternJP: Regex = "^ja_.*".r
    val langPatternEN: Regex = "^en_.*".r

    //Analyze everything as simple sentences as Claims, not just sentenceType
    val inputSentenceForParser = InputSentenceForParser(List.empty[KnowledgeForParser], List(knowledgeForParser), ActionModeType.REGISTRATION_MODE.index)
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

  private def registKnowledgeImagesAndTables(knowledgeForParsers: List[KnowledgeForParser], transversalState: TransversalState): List[KnowledgeForParser] = Try {

    knowledgeForParsers.foldLeft(List.empty[KnowledgeForParser]) {
      (acc, x) => {
        val knowledgeForImages: List[KnowledgeForImage] = x.knowledge.knowledgeForImages.map(y => {          
          val json: String = Json.toJson(KnowledgeForImage(y.id, y.imageReference)).toString()
          val knowledgeForImageJson: String = ToposoidUtils.callComponent(json,
            conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
            conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
            "registerImage", transversalState)
          val registeredContentResult: RegisteredImageContentResult = Json.parse(knowledgeForImageJson).as[RegisteredImageContentResult]
          if (registeredContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registeredContentResult.statusInfo.message)
          registeredContentResult.knowledgeForImage
        })
        val knowledgeForTables: List[KnowledgeForTable] = x.knowledge.knowledgeForTables.map(y => {          
          val json: String = Json.toJson(KnowledgeForTable(y.id, y.tableReference)).toString()
          val knowledgeForTableJson: String = ToposoidUtils.callComponent(json,
            conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
            conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
            "registerTable", transversalState)
          val registeredContentResult: RegisteredTableContentResult = Json.parse(knowledgeForTableJson).as[RegisteredTableContentResult]
          if (registeredContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registeredContentResult.statusInfo.message)
          registeredContentResult.knowledgeForTable
           
        })
        val knowledge = Knowledge(sentence = x.knowledge.sentence,
          lang = x.knowledge.lang, extentInfoJson = x.knowledge.extentInfoJson,
          isNegativeSentence = x.knowledge.isNegativeSentence, knowledgeForImages,
          knowledgeForTables, x.knowledge.knowledgeForDocument, x.knowledge.documentPageReference)
        acc :+ KnowledgeForParser(x.propositionId, x.sentenceId, knowledge)
      }
    }
  } match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  def registerData(knowledgeSentenceSetForParser: KnowledgeSentenceSetForParser, transversalState: TransversalState, addVectorFlag: Boolean = true, neo4JUtilsObject:Neo4JUtils = null): Unit = {

    val knowledgeSentenceSetForParserWithImage = KnowledgeSentenceSetForParser(
      registKnowledgeImagesAndTables(knowledgeSentenceSetForParser.premiseList, transversalState),
      knowledgeSentenceSetForParser.premiseLogicRelation,
      registKnowledgeImagesAndTables(knowledgeSentenceSetForParser.claimList, transversalState),
      knowledgeSentenceSetForParser.claimLogicRelation)

    val analyzedPropositionSet = getAnalyzedPropositionSet(knowledgeSentenceSetForParserWithImage, transversalState)
    Sentence2Neo4jTransformer.createGraph(analyzedPropositionSet, transversalState, neo4JUtilsObject = neo4JUtilsObject)
    if (addVectorFlag) FeatureVectorizer.createVector(knowledgeSentenceSetForParserWithImage, transversalState)
  }


  def deleteData(knowledgeSentenceSetForParser: KnowledgeSentenceSetForParser, transversalState: TransversalState) = {

    (knowledgeSentenceSetForParser.premiseList ::: knowledgeSentenceSetForParser.claimList).foreach(knowledgeForParser => {
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
      //FeatureVectorizer.removeVectorByPropositionId(knowledgeForParser, transversalState)
      FeatureVectorizer.removeAllVectorByDocumentId(knowledgeForParser.knowledge.knowledgeForDocument.id, List(knowledgeForParser.propositionId), transversalState)
    })
  }
  
  def analyzeByBaseDeductionUnit(asosJson:String, transversalState: TransversalState):String = {
  
    val host = Json.parse(conf.getString("TOPOSOID_CLAUSE_DEDUCTION_UNITS")).as[List[String]].head
    val port = Json.parse(conf.getString("TOPOSOID_CLAUSE_DEDUCTION_PORTS")).as[List[String]].head

    //val json = ToposoidUtils.callComponent(asosJson, conf.getString("TOPOSOID_DEDUCTION_UNIT1_HOST"), conf.getString("TOPOSOID_DEDUCTION_UNIT1_PORT"), "execute", transversalState)
    val json = ToposoidUtils.callComponent(asosJson, host, port, "execute", transversalState)
    val verifyingEdges = Json.parse(json).as[List[VerifyingEdges]]
    val analyzedSentenceObjects = Json.parse(asosJson).as[AnalyzedSentenceObjects]
    val asos = analyzedSentenceObjects.analyzedSentenceObjects
    
    val updatedAsos = asos.foldLeft(List.empty[AnalyzedSentenceObject]){
      (acc, x) => {
        val coveredPropositionEdges = verifyingEdges.filter(y => y.sentenceId.equals(x.knowledgeBaseSemiGlobalNode.sentenceId)).head.coveredPropositionEdges
        val updatedDeductionReult = DeductionResult(
          status = x.deductionResult.status, 
          authenticityType = x.deductionResult.authenticityType, 
          coveredPropositionEdges = coveredPropositionEdges, 
          evidenceKnowledgeList = x.deductionResult.evidenceKnowledgeList, 
          havePremiseInGivenProposition = x.deductionResult.havePremiseInGivenProposition, 
          deductionPhaseType = x.deductionResult.deductionPhaseType
        )        
        acc :+ AnalyzedSentenceObject(x.nodeMap, x.edgeList, x.knowledgeBaseSemiGlobalNode, updatedDeductionReult)
      }
    }
    Json.toJson(AnalyzedSentenceObjects(updatedAsos, analyzedSentenceObjects.deductionConfiguration)).toString    
  }


  def analyzeByBaseDeductionUnitForSemiGlobal(asosJson:String, transversalState: TransversalState):String = {
  
    val host = Json.parse(conf.getString("TOPOSOID_EMBEDDING_DEDUCTION_UNITS")).as[List[String]].head
    val port = Json.parse(conf.getString("TOPOSOID_EMBEDDING_DEDUCTION_PORTS")).as[List[String]].head

    val json = ToposoidUtils.callComponent(asosJson, host, port, "execute", transversalState)
    val verifyingEdges = Json.parse(json).as[List[VerifyingEdges]]
    val analyzedSentenceObjects = Json.parse(asosJson).as[AnalyzedSentenceObjects]
    val asos = analyzedSentenceObjects.analyzedSentenceObjects
    
    val updatedAsos = asos.foldLeft(List.empty[AnalyzedSentenceObject]){
      (acc, x) => {
        val coveredPropositionEdges = verifyingEdges.filter(y => y.sentenceId.equals(x.knowledgeBaseSemiGlobalNode.sentenceId)).head.coveredPropositionEdges
        val updatedDeductionReult = DeductionResult(
          status = x.deductionResult.status, 
          authenticityType = x.deductionResult.authenticityType, 
          coveredPropositionEdges = coveredPropositionEdges, 
          evidenceKnowledgeList = x.deductionResult.evidenceKnowledgeList, 
          havePremiseInGivenProposition = x.deductionResult.havePremiseInGivenProposition, 
          deductionPhaseType = x.deductionResult.deductionPhaseType
        )        
        acc :+ AnalyzedSentenceObject(x.nodeMap, x.edgeList, x.knowledgeBaseSemiGlobalNode, updatedDeductionReult)
      }
    }
    Json.toJson(AnalyzedSentenceObjects(updatedAsos, analyzedSentenceObjects.deductionConfiguration)).toString
    
  }

  def checkMatchedBothSide(json:String, sentenceId:String, verifyingEdgesList:List[VerifyingEdges], correctSize:Int ):Unit = {

      val evalA:VerifyingEdges = verifyingEdgesList.filter(x => x.sentenceId.equals(sentenceId)).head
      val coveredEdges = evalA.coveredPropositionEdges.filter(x => x.destinationNode.isConfirmed && x.sourceNode.isConfirmed)
      assert(coveredEdges.size == correctSize)
      if(coveredEdges.size == 0) return
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]
      //両側被覆エッジに含まれるノードのチェック
      val targetAso = analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceId.equals(sentenceId)).head      
      coveredEdges.foreach(x => {
        assert(targetAso.nodeMap.get(x.sourceNode.terminalId).get.predicateArgumentStructure.surface.equals(x.sourceNode.terminalSurface))
        assert(targetAso.nodeMap.get(x.destinationNode.terminalId).get.predicateArgumentStructure.surface.equals(x.destinationNode.terminalSurface))        
      })

      val sentenceIds = coveredEdges.foldLeft(List.empty[String]){
        (acc, x) => {        
          val sourceKnowledgeSentenceIds = x.sourceNode.matchedKnowledgeNodes.foldLeft(Set.empty[String]){(acc2, y) => {
            acc2 + y.sentenceId
          }}        
          val destinationKnowledgeSentenceIds = x.destinationNode.matchedKnowledgeNodes.foldLeft(Set.empty[String]){(acc2, y) => {
            acc2 + y.sentenceId
          }}
          val targetSentenceIds = sourceKnowledgeSentenceIds & destinationKnowledgeSentenceIds 
          assert(targetSentenceIds.size > 0)
          acc ::: targetSentenceIds.toList
        }
      }            
      assert(sentenceIds.groupBy(identity).filter(x => x._2.size >= correctSize).size > 0)
  }


  def checkMatchedOneSide(json:String, sentenceId:String, verifyingEdgesList:List[VerifyingEdges], correctSize:Int ):Unit = {

      val evalA:VerifyingEdges = verifyingEdgesList.filter(x => x.sentenceId.equals(sentenceId)).head
      val coveredEdges = evalA.coveredPropositionEdges.filter(x => (x.destinationNode.isConfirmed || x.sourceNode.isConfirmed) && !(x.destinationNode.isConfirmed && x.sourceNode.isConfirmed))
      assert(coveredEdges.size == correctSize)
      if(coveredEdges.size == 0) return
      val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]
      //両側被覆エッジに含まれるノードのチェック
      val targetAso = analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceId.equals(sentenceId)).head      
      coveredEdges.foreach(x => {
        if(x.sourceNode.isConfirmed){
          assert(targetAso.nodeMap.get(x.sourceNode.terminalId).get.predicateArgumentStructure.surface.equals(x.sourceNode.terminalSurface))
        }
        if(x.destinationNode.isConfirmed){
          assert(targetAso.nodeMap.get(x.destinationNode.terminalId).get.predicateArgumentStructure.surface.equals(x.destinationNode.terminalSurface))        
        }        
      })

      val sentenceIds = coveredEdges.foldLeft(List.empty[String]){
        (acc, x) => {           
          val sourceKnowledgeSentenceIds = x.sourceNode.isConfirmed match {
            case true => {
              x.sourceNode.matchedKnowledgeNodes.foldLeft(Set.empty[String]){(acc2, y) => {
                acc2 + y.sentenceId
              }}
            }
            case _ => {
              Set.empty[String]
            }
          }
          val destinationKnowledgeSentenceIds = x.destinationNode.isConfirmed match {
            case true => {
              x.destinationNode.matchedKnowledgeNodes.foldLeft(Set.empty[String]){(acc2, y) => {
                acc2 + y.sentenceId
              }}
            }
            case _ => {
              Set.empty[String]
            }
          }
          val targetSentenceIds = sourceKnowledgeSentenceIds | destinationKnowledgeSentenceIds 
          if((x.sourceNode.isConfirmed || x.destinationNode.isConfirmed) && !(x.sourceNode.isConfirmed && x.destinationNode.isConfirmed) ){
            assert(targetSentenceIds.size > 0)
          }        
          acc ::: targetSentenceIds.toList
        }
      }      
      assert(sentenceIds.groupBy(identity).filter(x => x._2.size >= correctSize).size > 0)
  }


  def checkNoMatch(json:String, sentenceId:String, verifyingEdgesList:List[VerifyingEdges], correctSize:Int ):Unit = {
      val evalA:VerifyingEdges = verifyingEdgesList.filter(x => x.sentenceId.equals(sentenceId)).head
      val coveredEdges = evalA.coveredPropositionEdges.filter(x => !x.destinationNode.isConfirmed && !x.sourceNode.isConfirmed)
      assert(coveredEdges.size == correctSize)
  }

  def checkMatchedFuzzy(json:String, sentenceId:String, verifyingEdgesList:List[VerifyingEdges], correctSize:Int ):Unit = {

    val evalA:VerifyingEdges = verifyingEdgesList.filter(x => x.sentenceId.equals(sentenceId)).head
    val coveredEdges = evalA.coveredPropositionEdges.filter(x => !x.destinationNode.isConfirmed && !x.sourceNode.isConfirmed && x.sourceNode.matchedKnowledgeNodes.size + x.destinationNode.matchedKnowledgeNodes.size > 0)        
    assert(coveredEdges.size == correctSize)
    if(coveredEdges.size == 0) return
    
    val analyzedSentenceObjects: AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]
    //両側被覆エッジに含まれるノードのチェック
    val targetAso = analyzedSentenceObjects.analyzedSentenceObjects.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceId.equals(sentenceId)).head      
    coveredEdges.foreach(x => {
      
      if(!x.sourceNode.isConfirmed){      
        assert(targetAso.nodeMap.get(x.sourceNode.terminalId).get.predicateArgumentStructure.surface.equals(x.sourceNode.terminalSurface))
      }
      if(!x.destinationNode.isConfirmed){
        assert(targetAso.nodeMap.get(x.destinationNode.terminalId).get.predicateArgumentStructure.surface.equals(x.destinationNode.terminalSurface))        
      }        
    })

    val sentenceIds = coveredEdges.foldLeft(List.empty[String]){
      (acc, x) => {      
        //評価されたエッジがあるということは、ノードの関係性を保持しており、nodeIdを指定すれば必ず一意に決まる。    
        //命題サイドのエッジのcaseNameを特定
        val targetEdges = targetAso.edgeList.filter(z => {
          z.sourceId.equals(x.sourceNode.terminalId) &&
          z.destinationId.equals(x.destinationNode.terminalId)
        })              
        assert(targetEdges.size == 1)

        val sourceKnowledgeSentenceIds = !x.sourceNode.isConfirmed match {
          case true => {
            x.sourceNode.matchedKnowledgeNodes.foldLeft(Set.empty[String]){(acc2, y) => { 
              //ノード間の関係性のみチェック             
              assert(y.caseNameOnEdge.equals(targetEdges.head.caseStr))
              acc2 + y.sentenceId
            }}
          }
          case _ => {
            Set.empty[String]
          }
        }
        val destinationKnowledgeSentenceIds = !x.destinationNode.isConfirmed match {
          case true => {
            x.destinationNode.matchedKnowledgeNodes.foldLeft(Set.empty[String]){(acc2, y) => {
              //ノード間の関係性のみチェック
              assert(y.caseNameOnEdge.equals(targetEdges.head.caseStr))
              acc2 + y.sentenceId
            }}
          }
          case _ => {
            Set.empty[String]
          }
        }
        val targetSentenceIds = sourceKnowledgeSentenceIds & destinationKnowledgeSentenceIds 
        if(!x.sourceNode.isConfirmed && !x.destinationNode.isConfirmed){
          assert(targetSentenceIds.size > 0)
        }        
        acc ::: targetSentenceIds.toList
      }
    }      
    assert(sentenceIds.groupBy(identity).filter(x => x._2.size >= correctSize).size > 0)
  }

  private def chooseDeductionUnitEndPoints(prefixEnvName:String, selectIndice:List[Int]):Seq[Endpoint] = {
    val hosts = Json.parse(conf.getString(s"${prefixEnvName}_DEDUCTION_UNITS")).as[List[String]]
    val ports = Json.parse(conf.getString(s"${prefixEnvName}_DEDUCTION_PORTS")).as[List[String]]
    val names = Json.parse(conf.getString(s"${prefixEnvName}_DEDUCTION_NAMES")).as[List[String]]
    hosts.zipWithIndex.lazyZip(ports).lazyZip(names).map { 
      case ((x, idx), y, z) => 
        selectIndice.size match {
          case 0 => Option(Endpoint(x,y,z))
          case _ => {
            if (selectIndice.contains(idx)) Option(Endpoint(x,y,z)) else None
          }
        }            
    }.toSeq.flatten
  }

  def setDeductionUnitEndPoints(deductionPhaseType:DeductionPhaseType, transversalState:TransversalState, selectIndice:List[Int] = List.empty[Int]):Unit = {
    deductionPhaseType match {
      case DeductionPhaseType.DEDUCTION_SENTENCE_BASE => {      
        val endPoints = chooseDeductionUnitEndPoints("TOPOSOID_EMBEDDING", selectIndice)
        InMemoryDbUtils.setEmbedingDeducitonUnitEndPoints(endPoints, transversalState)  
      }
      case DeductionPhaseType.DEDUCTION_TERM_BASE => {        
        val endPoints = chooseDeductionUnitEndPoints("TOPOSOID_CLAUSE", selectIndice)
        InMemoryDbUtils.setClauseDeducitonUnitEndPoints(endPoints, transversalState)  
      }
      case DeductionPhaseType.DEDUCTION_PHRASE_BASE => {
        val endPoints = chooseDeductionUnitEndPoints("TOPOSOID_HYBRID", selectIndice)
        //InMemoryDbUtils.setClauseDeducitonUnitEndPoints(endPoints, transversalState)  
      }
    }
  }

  private def isUrl(input: String): Boolean = {
    Try {
      val uri = URI.create(input)
      val scheme = uri.getScheme
      scheme != null && (scheme.equalsIgnoreCase("http") || scheme.equalsIgnoreCase("https"))
    }.getOrElse(false)
  }

  def deleteFeatureVector(featureVectorIdentifier: FeatureVectorIdentifier, featureType: FeatureType, transversalState:TransversalState):Unit = {
    val json: String = Json.toJson(featureVectorIdentifier).toString()
    if(featureType.equals(FeatureType.SENTENCE)){
      ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "delete", transversalState)
    }else if(featureType.equals(FeatureType.IMAGE)){
      ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_IMAGE_VECTORDB_ACCESSOR_PORT"), "delete", transversalState)
    }
  }

  def getImageVector(url: String, transversalState:TransversalState): FeatureVector = {
    val singleImage = SingleImage(url)
    val json: String = Json.toJson(singleImage).toString()
    val featureVectorJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_COMMON_IMAGE_RECOGNITION_HOST"), conf.getString("TOPOSOID_COMMON_IMAGE_RECOGNITION_PORT"), "getFeatureVector", transversalState)
    Json.parse(featureVectorJson).as[FeatureVector]
  }

  def getTableVector(url: String, transversalState:TransversalState): FeatureVector = {
    val singleTable = SingleTable(url)
    val json: String = Json.toJson(singleTable).toString()
    val featureVectorJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_COMMON_TABLE_RECOGNITION_HOST"), conf.getString("TOPOSOID_COMMON_TABLE_RECOGNITION_PORT"), "getFeatureVector", transversalState)
    Json.parse(featureVectorJson).as[FeatureVector]
  }

  def uploadImage(knowledgeForImage: KnowledgeForImage, transversalState: TransversalState): KnowledgeForImage = {
    
    val endpoint = "http://" + conf.getString("TOPOSOID_FILE_UPLOAD_FACADE_HOST") + ":" + conf.getString("TOPOSOID_FILE_UPLOAD_FACADE_PORT") + "/upload"    
    val backend = DefaultSyncBackend(
      options = BackendOptions.connectionTimeout(1.minute))
    val request = isUrl(knowledgeForImage.imageReference.reference.originalUrlOrReference) match {
      case true => {
        basicRequest
        .header(TRANSVERSAL_STATE.str, Json.toJson(transversalState).toString())      
        .httpVersion(HttpVersion.HTTP_1_1)
        .post(uri"${endpoint}") // Replace with your upload endpoint
        .multipartBody(
            multipart("featureType", FeatureType.IMAGE.index.toString),
            multipart("url", knowledgeForImage.imageReference.reference.originalUrlOrReference),
        )
      }
      case _ => {
        val file: Path = Paths.get(knowledgeForImage.imageReference.reference.originalUrlOrReference)
        basicRequest
        .header(TRANSVERSAL_STATE.str, Json.toJson(transversalState).toString())      
        .httpVersion(HttpVersion.HTTP_1_1)
        .post(uri"${endpoint}") // Replace with your upload endpoint
        .multipartBody(
            multipart("featureType", FeatureType.IMAGE.index.toString),
            multipart("url", ""), 
            multipartFile("uploadfile", file.toFile()).fileName(file.getFileName().toString()).contentType("application/octet-stream") // "file" is the field name on the server         
        )
      }
    }
    val response = request.send(backend)
    val responseJson = response.body match {
      case Right(successBody) => s"$successBody"
      case Left(errorBody) => s"Upload failed. Status code: ${response.code}. Error body: $errorBody"
    }
    val uploadResult = Json.parse(responseJson).as[UploadResult]
    val imageReferenceOrg = knowledgeForImage.imageReference.reference
    val reference = Reference(url = uploadResult.url, surface = imageReferenceOrg.surface, surfaceIndex = imageReferenceOrg.surfaceIndex, isWholeSentence = imageReferenceOrg.isWholeSentence, originalUrlOrReference = knowledgeForImage.imageReference.reference.originalUrlOrReference, metaInformations = List.empty[String])
    val imageReference = ImageReference(reference = reference, x = 0, y = 0, width = 640, height = 480)
    KnowledgeForImage(id = uploadResult.id, imageReference = imageReference)
  }

  def uploadTable(knowledgeForTable: KnowledgeForTable, transversalState: TransversalState): KnowledgeForTable = {

    val endpoint = "http://" + conf.getString("TOPOSOID_FILE_UPLOAD_FACADE_HOST") + ":" + conf.getString("TOPOSOID_FILE_UPLOAD_FACADE_PORT") + "/upload"    
    val backend = DefaultSyncBackend(
      options = BackendOptions.connectionTimeout(1.minute))
    val request = isUrl(knowledgeForTable.tableReference.reference.originalUrlOrReference) match {
      case true => {
        basicRequest
        .header(TRANSVERSAL_STATE.str, Json.toJson(transversalState).toString())      
        .httpVersion(HttpVersion.HTTP_1_1)
        .post(uri"${endpoint}") // Replace with your upload endpoint
        .multipartBody(
            multipart("featureType", FeatureType.TABLE.index.toString),
            multipart("url", knowledgeForTable.tableReference.reference.originalUrlOrReference), // デフォルト値を明示的に送る場合     
        )
      }
      case _ => {
        val file: Path = Paths.get(knowledgeForTable.tableReference.reference.originalUrlOrReference)
        basicRequest
        .header(TRANSVERSAL_STATE.str, Json.toJson(transversalState).toString())      
        .httpVersion(HttpVersion.HTTP_1_1)
        .post(uri"${endpoint}") // Replace with your upload endpoint
        .multipartBody(
            multipart("featureType", FeatureType.TABLE.index.toString),
            multipart("url", ""), // デフォルト値を明示的に送る場合     
            multipartFile("uploadfile", file.toFile()).fileName(file.getFileName().toString()).contentType("application/octet-stream") // "file" is the field name on the server         
        )
      }
    }            
    val response = request.send(backend)
    val responseJson = response.body match {
      case Right(successBody) => s"$successBody"
      case Left(errorBody) => s"Upload failed. Status code: ${response.code}. Error body: $errorBody"
    }
    val uploadResult = Json.parse(responseJson).as[UploadResult]    
    val tableReferenceOrg = knowledgeForTable.tableReference.reference
    val reference = Reference(url = uploadResult.url, surface = tableReferenceOrg.surface, surfaceIndex = tableReferenceOrg.surfaceIndex, isWholeSentence = tableReferenceOrg.isWholeSentence, originalUrlOrReference = knowledgeForTable.tableReference.reference.originalUrlOrReference, metaInformations = List.empty[String])
    val tableReference = TableReference(reference=reference, skipHeaderRows = knowledgeForTable.tableReference.skipHeaderRows, skipRowList = knowledgeForTable.tableReference.skipRowList, multiHeaderRows =  knowledgeForTable.tableReference.multiHeaderRows, sheetNameForExcel =  knowledgeForTable.tableReference.sheetNameForExcel)
    KnowledgeForTable(id = uploadResult.id, tableReference = tableReference)

  }  


  def getAnalyzedSentenceObjectsJsonForSemiGlobal(lang:String,inputSentenceForParser: InputSentenceForParser, transversalState:TransversalState/*, knowledgeForImages:List[KnowledgeForImage]=List.empty[KnowledgeForImage], knowledgeForTables:List[KnowledgeForTable]=List.empty[KnowledgeForTable]*/): String = {
    
    val inputSentenceForParserJson = Json.toJson(inputSentenceForParser).toString
    val json = lang match {
      case "ja_JP" => ToposoidUtils.callComponent(inputSentenceForParserJson, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      case "en_US" => ToposoidUtils.callComponent(inputSentenceForParserJson, conf.getString("TOPOSOID_SENTENCE_PARSER_EN_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_EN_WEB_PORT"), "analyze", transversalState)
    }

    val asos: AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]
    val updatedAsos = asos.analyzedSentenceObjects.foldLeft(List.empty[AnalyzedSentenceObject]) {      
      (acc, x) => {

        val targetKnoledge = (inputSentenceForParser.premise ::: inputSentenceForParser.claim).filter(y => y.sentenceId.equals(x.knowledgeBaseSemiGlobalNode.sentenceId)).head

        
        val knowledgeFeatureReferenceImage: List[KnowledgeFeatureReference] = targetKnoledge.knowledge.knowledgeForImages.map(y => {          
          val json: String = Json.toJson(y).toString()
          val knowledgeForImageJson: String = ToposoidUtils.callComponent(json,
            conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
            conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
            "convertImage", transversalState)
          val registeredContentResult: RegisteredImageContentResult = Json.parse(knowledgeForImageJson).as[RegisteredImageContentResult]
          if (registeredContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registeredContentResult.statusInfo.message)          
          KnowledgeFeatureReference(
            propositionId = x.knowledgeBaseSemiGlobalNode.propositionId,
            sentenceId = x.knowledgeBaseSemiGlobalNode.sentenceId,
            featureId = registeredContentResult.knowledgeForImage.id,
            featureType = FeatureType.IMAGE.index,
            url = registeredContentResult.knowledgeForImage.imageReference.reference.url,
            source = registeredContentResult.knowledgeForImage.imageReference.reference.originalUrlOrReference,
            featureInputType = DataEntryType.MANUAL.index)        
        })

        val knowledgeFeatureReferenceTable: List[KnowledgeFeatureReference] = targetKnoledge.knowledge.knowledgeForTables.map(y => {          
          val json: String = Json.toJson(KnowledgeForTable(y.id, y.tableReference)).toString()
          val knowledgeForTableJson: String = ToposoidUtils.callComponent(json,
            conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
            conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
            "convertTable", transversalState)
          val registeredContentResult: RegisteredTableContentResult = Json.parse(knowledgeForTableJson).as[RegisteredTableContentResult]
          if (registeredContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registeredContentResult.statusInfo.message)
          KnowledgeFeatureReference(
            propositionId = x.knowledgeBaseSemiGlobalNode.propositionId,
            sentenceId = x.knowledgeBaseSemiGlobalNode.sentenceId,
            featureId = registeredContentResult.knowledgeForTable.id,
            featureType = FeatureType.IMAGE.index,
            url = registeredContentResult.knowledgeForTable.tableReference.reference.url,
            source = registeredContentResult.knowledgeForTable.tableReference.reference.originalUrlOrReference,
            featureInputType = DataEntryType.MANUAL.index)                   
        })


        val localContextForFeature = LocalContextForFeature(
          x.knowledgeBaseSemiGlobalNode.localContextForFeature.lang,knowledgeFeatureReferenceImage:::knowledgeFeatureReferenceTable)

        val knowledgeBaseSemiGlobalNode = KnowledgeBaseSemiGlobalNode(
          sentenceId = x.knowledgeBaseSemiGlobalNode.sentenceId,
          propositionId = x.knowledgeBaseSemiGlobalNode.propositionId,
          documentId = x.knowledgeBaseSemiGlobalNode.documentId,
          sentence = x.knowledgeBaseSemiGlobalNode.sentence,
          sentenceType = x.knowledgeBaseSemiGlobalNode.sentenceType,
          localContextForFeature = localContextForFeature)

        acc :+ AnalyzedSentenceObject(
          nodeMap = x.nodeMap,
          edgeList = x.edgeList,
          knowledgeBaseSemiGlobalNode = knowledgeBaseSemiGlobalNode,
          deductionResult = x.deductionResult)
      }
    }
    Json.toJson(AnalyzedSentenceObjects(updatedAsos, asos.deductionConfiguration)).toString()
  }

  def getAnalyzedSentenceObjectsJson(lang:String,inputSentenceForParser: InputSentenceForParser, transversalState:TransversalState) :String = {
    
    val inputSentenceForParserJson = Json.toJson(inputSentenceForParser).toString

    val json = lang match {
      case "ja_JP" => ToposoidUtils.callComponent(inputSentenceForParserJson, conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_JP_WEB_PORT"), "analyze", transversalState)
      case "en_US" => ToposoidUtils.callComponent(inputSentenceForParserJson, conf.getString("TOPOSOID_SENTENCE_PARSER_EN_WEB_HOST"), conf.getString("TOPOSOID_SENTENCE_PARSER_EN_WEB_PORT"), "analyze", transversalState)
    }    
    val asos: AnalyzedSentenceObjects = Json.parse(json).as[AnalyzedSentenceObjects]
    val updatedAsos = asos.analyzedSentenceObjects.foldLeft(List.empty[AnalyzedSentenceObject]) {
      (acc, x) => {
        val nodeMap = x.nodeMap.foldLeft(Map.empty[String, KnowledgeBaseNode]) {
          (acc2, y) => {

            val targetKnoledge = (inputSentenceForParser.premise ::: inputSentenceForParser.claim).filter(y => y.sentenceId.equals(x.knowledgeBaseSemiGlobalNode.sentenceId)).head
            
             val compatibleImages = targetKnoledge.knowledge.knowledgeForImages.filter(z => {
              z.imageReference.reference.surface == y._2.predicateArgumentStructure.surface && z.imageReference.reference.surfaceIndex == y._2.predicateArgumentStructure.currentId
            })         

            val knowledgeFeatureReferenceImages: List[KnowledgeFeatureReference] = compatibleImages.map(z => {          
              val json: String = Json.toJson(z).toString()
              val knowledgeForImageJson: String = ToposoidUtils.callComponent(json,
                conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
                conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
                "convertImage", transversalState)
              val registeredContentResult: RegisteredImageContentResult = Json.parse(knowledgeForImageJson).as[RegisteredImageContentResult]
              if (registeredContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registeredContentResult.statusInfo.message)          
              KnowledgeFeatureReference(
                propositionId = y._2.propositionId,
                sentenceId = y._2.sentenceId,
                featureId = registeredContentResult.knowledgeForImage.id,
                featureType = FeatureType.IMAGE.index,
                url = registeredContentResult.knowledgeForImage.imageReference.reference.url,
                source = registeredContentResult.knowledgeForImage.imageReference.reference.originalUrlOrReference,
                featureInputType = DataEntryType.MANUAL.index)        
            })

             val compatibleTables = targetKnoledge.knowledge.knowledgeForTables.filter(z => {
              z.tableReference.reference.surface == y._2.predicateArgumentStructure.surface && z.tableReference.reference.surfaceIndex == y._2.predicateArgumentStructure.currentId
            })          

            val knowledgeFeatureReferenceTables: List[KnowledgeFeatureReference] = compatibleTables.map(z => {          
              val json: String = Json.toJson(z).toString()
              val knowledgeForTableJson: String = ToposoidUtils.callComponent(json,
                conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
                conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
                "convertTable", transversalState)
              val registeredContentResult: RegisteredTableContentResult = Json.parse(knowledgeForTableJson).as[RegisteredTableContentResult]
              if (registeredContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registeredContentResult.statusInfo.message)
              KnowledgeFeatureReference(
                propositionId = y._2.propositionId,
                sentenceId = y._2.sentenceId,
                featureId = registeredContentResult.knowledgeForTable.id,
                featureType = FeatureType.TABLE.index,
                url = registeredContentResult.knowledgeForTable.tableReference.reference.url,
                source = registeredContentResult.knowledgeForTable.tableReference.reference.originalUrlOrReference,
                featureInputType = DataEntryType.MANUAL.index)                   
            })


            val knowledgeBaseNode = KnowledgeBaseNode(
              nodeId = y._2.nodeId,
              propositionId = y._2.propositionId,
              sentenceId = y._2.sentenceId,
              predicateArgumentStructure = y._2.predicateArgumentStructure,
              localContext = LocalContext(
                lang = y._2.localContext.lang,
                namedEntities = y._2.localContext.namedEntities,
                rangeExpressions = y._2.localContext.rangeExpressions,
                categories = y._2.localContext.categories,
                domains = y._2.localContext.domains,
                knowledgeFeatureReferences = knowledgeFeatureReferenceImages:::knowledgeFeatureReferenceTables,
                properNouns = y._2.localContext.properNouns)
                )
            acc2 ++ Map(y._1 -> knowledgeBaseNode)
          }
        }
        acc :+ AnalyzedSentenceObject(
          nodeMap = nodeMap,
          edgeList = x.edgeList,
          knowledgeBaseSemiGlobalNode = x.knowledgeBaseSemiGlobalNode,
          deductionResult = x.deductionResult)
      }
    }
    Json.toJson(AnalyzedSentenceObjects(updatedAsos, asos.deductionConfiguration)).toString()    
  }
}
