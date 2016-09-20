package edu.colorado.plv.aiwala

import com.ibm.wala.ipa.callgraph._
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.util.config.AnalysisScopeReader
import com.ibm.wala.ssa.IR
import com.ibm.wala.ipa.callgraph.AnalysisScope
import scala.collection.JavaConversions._
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.callgraph.AnalysisOptions.ReflectionOptions


object WalaClient {

  lazy private val cache = new AnalysisCache

  //Generate WALA IR for (@param app)'s main method, without generating a callgraph for the whole program
  def getIR(app: String): Option[IR] =
    ClassHierarchy.make(AnalysisScopeReader.makeJavaBinaryAnalysisScope(app, null))
      .find { klass => !(klass.toString contains "Primordial") }
      .flatMap { klass => klass.getAllMethods.find { method => method.toString contains "main" } }
      .map {
        cache.getIR
      }
  }

  object main {
    def main(args: Array[String]): Unit = {
      if(args.length < 3){
        println("usage: cmd [classv1 file] [classv2 file] [source variable number]")
      }
      val version1Class = args(0)
      val version2Class = args(1)
      val source = args(2)
      val SA = new SliceAnalysis(WalaClient.getIR(version1Class).get,source.toInt)
      System.out.println("Original Instructions:")
      SA.printInstructions()
      var state: State = SA.execute(SA.initialState)
      println("Slice:" + state.slice)
      val IA = new IncrementalAnalysis(WalaClient.getIR(version1Class).get, WalaClient.getIR(version2Class).get)
      System.out.println("Revised Instructions:")
      IA.printInstructions()
      state = IA.execute(state);
      println("Revised Slice:" + state.slice)
      val NA = new SliceAnalysis(WalaClient.getIR(version2Class).get,source.toInt)
      state = NA.execute(NA.initialState)
      println("From Scratch Slice:" + state.slice)
    }
}