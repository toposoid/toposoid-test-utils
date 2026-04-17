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
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.RegistContentResult
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
          //val imageFeatureId = UUID.random.toString
          val json: String = Json.toJson(KnowledgeForImage(y.id, y.imageReference)).toString()
          val knowledgeForImageJson: String = ToposoidUtils.callComponent(json,
            conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
            conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
            "registImage", transversalState)
          val registContentResult: RegistContentResult = Json.parse(knowledgeForImageJson).as[RegistContentResult]
          if (registContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registContentResult.statusInfo.message)
          registContentResult.knowledgeForImage
        })
        val knowledgeForTable: List[KnowledgeForTable] = x.knowledge.knowledgeForTables.map(y => {
          y
          //TODO implementation
          /*
          val imageFeatureId = UUID.random.toString
          val json: String = Json.toJson(KnowledgeForTable(imageFeatureId, y.tableReference)).toString()
          val knowledgeForTableJson: String = ToposoidUtils.callComponent(json,
            conf.getString("TOPOSOID_CONTENTS_ADMIN_HOST"),
            conf.getString("TOPOSOID_CONTENTS_ADMIN_PORT"),
            "registTable", transversalState)
          val registContentResult: RegistContentResult = Json.parse(knowledgeForTableJson).as[RegistContentResult]
          if (registContentResult.statusInfo.status.equals("ERROR")) throw new Exception(registContentResult.statusInfo.message)
          registContentResult.knowledgeForTable
           */
        })
        val knowledge = Knowledge(sentence = x.knowledge.sentence,
          lang = x.knowledge.lang, extentInfoJson = x.knowledge.extentInfoJson,
          isNegativeSentence = x.knowledge.isNegativeSentence, knowledgeForImages,
          x.knowledge.knowledgeForTables /*TODO implementation for knowledgeForTables*/, x.knowledge.knowledgeForDocument, x.knowledge.documentPageReference)
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
  
    val json = ToposoidUtils.callComponent(asosJson, conf.getString("TOPOSOID_DEDUCTION_UNIT1_HOST"), conf.getString("TOPOSOID_DEDUCTION_UNIT1_PORT"), "execute", transversalState)
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

}
