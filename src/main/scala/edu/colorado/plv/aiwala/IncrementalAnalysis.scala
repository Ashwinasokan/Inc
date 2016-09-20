package edu.colorado.plv.aiwala

import com.ibm.wala.ssa.IR
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa._
import com.ibm.wala.types.FieldReference
import scala.collection.JavaConversions._
import com.ibm.wala.types.MethodReference
import com.ibm.wala.shrikeBT.IBinaryOpInstruction
import com.ibm.wala.shrikeBT.IConditionalBranchInstruction

import scala.collection.mutable.ListBuffer
class IncrementalAnalysis(old: IR, ir: IR){

  var deleted = Set[SSAInstruction]()

  def getInstructions : Array[SSAInstruction] = {
    ir.getControlFlowGraph.flatMap {bb => bb.iterator}.toArray
  }

  def getOldInstructions : Array[SSAInstruction] = {
    old.getControlFlowGraph.flatMap {bb => bb.iterator}.filterNot(deleted).toArray
  }

  def nextInstrMap : Map[Int,SSAInstruction] = {
    val instrs : Array[SSAInstruction] = getInstructions
    (0 to instrs.length - 2).map {i => (instrs(i).hashCode,instrs(i+1))}.toMap
  }

  def previousInstrMap : Map[Int,SSAInstruction] = {
    val instrs : Array[SSAInstruction] = getInstructions.reverse
    (0 to instrs.length - 2).map {i => (instrs(i).hashCode,instrs(i+1))}.toMap
  }

  def oldPreviousInstrMap : Map[Int,SSAInstruction] = {
    val instrs : Array[SSAInstruction] = getOldInstructions.reverse
    (0 to instrs.length - 2).map {i => (instrs(i).hashCode,instrs(i+1))}.toMap
  }

  def oldNextInstrMap : Map[Int,SSAInstruction] = {
    val instrs : Array[SSAInstruction] = getOldInstructions
    (0 to instrs.length - 2).map {i => (instrs(i).hashCode,instrs(i+1))}.toMap
  }

  def hasNextInstr(instr: SSAInstruction): Boolean = {
    nextInstrMap.contains(instr.hashCode)
  }

  def getNextInstr(instr: SSAInstruction): SSAInstruction = {
    nextInstrMap.get(instr.hashCode).get
  }
  def getPreviousInstr(instr: SSAInstruction): SSAInstruction = {
    previousInstrMap.get(instr.hashCode).get
  }

  def getOldPreviousInstr(instr: SSAInstruction): SSAInstruction = {
    oldPreviousInstrMap.get(instr.hashCode).get
  }

  def hasOldNextInstr(instr: SSAInstruction): Boolean = {
    oldNextInstrMap.contains(instr.hashCode)
  }

  def getOldNextInstr(instr: SSAInstruction): SSAInstruction = {
    oldNextInstrMap.get(instr.hashCode).get
  }

  def getInstrIndex(instruction:SSAInstruction): Int = {
    val instrs = ir.getControlFlowGraph.getInstructions()
    for (i <- 0 until instrs.length) {
      if (instruction == instrs(i)) {
        return i;
      }
    }
    throw new Exception("Instruction not found")
  }

  def getInstrAtIndex(index: Int): SSAInstruction = {
    val targetBlock = ir.getControlFlowGraph.find { bb => bb.getFirstInstructionIndex >= index && bb.iterator.hasNext}
    targetBlock match {
      case Some(bb) => bb.iterator.next
      case None => throw new Exception("Instruction not found")
    }
  }

  def calculateRelevance(curInstr:SSAInstruction,refs: Map[String, (SSAInstruction, Set[LocalVar])],
                         defs: Map[String, (SSAInstruction, Set[LocalVar])],lastInstrRelevants:Set[LocalVar]): (Set[LocalVar],Set[String]) = {
    if ((lastInstrRelevants intersect  refs.get(curInstr.toString).get._2 ).isEmpty) {
      (Set[LocalVar]() ++ lastInstrRelevants,Set[String]())
    }
    else {
      (Set[LocalVar]() ++ lastInstrRelevants ++ (defs.get(curInstr.toString).get._2),Set[String](curInstr.toString))
    }
  }

  def printInstructions() : Unit = {
    val instrs = getInstructions
    for(i <- 0 until instrs.length) {
      println(i + ")" + instrs(i))
    }
  }

  def stepOld (state: State): State = {
    var list = state.worklist
    val slice = state.slice
    val curInstr = list.head
    val lastInstr = state.lastInstr
    val relevants = state.relevants
    println(curInstr)
    println(lastInstr)
    val (curRelevance, curSlice) = calculateRelevance(curInstr, state.refs, state.defs, relevants.get(lastInstr.toString).get._2)
    val history = state.history
    val statics = state.statics
    val locals = state.locals
    if (list.isEmpty)
      return state
    if (hasOldNextInstr(curInstr)) {
      list = getOldNextInstr(curInstr) :: list.tail
    }
    else {
      list = list.tail
    }
    State(list, curInstr,history,slice ++ curSlice,statics, locals,state.heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
  }

  def step (state: State): State = {
    var list = state.worklist
    val slice = state.slice
    val curInstr = list.head
    val lastInstr = state.lastInstr
    val relevants = state.relevants
    val (curRelevance, curSlice) = calculateRelevance(curInstr, state.refs, state.defs, relevants.get(lastInstr.toString).get._2)
    val history = state.history
    val statics = state.statics
    val locals = state.locals
    val heap = state.heap
    if (list.isEmpty)
      return state
    if (hasNextInstr(curInstr)) {
      list = getNextInstr(curInstr) :: list.tail
    }
    else {
      list = list.tail
    }
    State(list, curInstr,history,slice ++ curSlice,statics, locals,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
  }

  def instrType(instr:SSAInstruction): String = {
    instr match {
      case ins: SSAReturnInstruction => "SSAReturnInstruction"
      case ins: SSAPhiInstruction => "SSAPhiInstruction"
      case ins: SSAPutInstruction => "SSAPutInstruction"
      case ins: SSAGetInstruction => "SSAGetInstruction"
      case ins: SSAGotoInstruction => "SSAGotoInstruction"
      case ins: SSABinaryOpInstruction => "SSABinaryOpInstruction" + ins.getOperator
      case ins: SSAConditionalBranchInstruction => "SSAConditionalBranchInstruction" +  ins.getOperator
      case ins: SSAArrayStoreInstruction => "SSAArrayStoreInstruction"
      case ins: SSAArrayLoadInstruction => "SSAArrayLoadInstruction"
      case ins: SSANewInstruction => "SSANewInstruction"
      case _=> throw new Exception("Unknown Instruction")
    }
  }

  def transform(previous:SSAInstruction,current:SSAInstruction): Map[LocalVar,LocalVar] = {
    (previous,current) match {
      case (p:SSAReturnInstruction,c:SSAReturnInstruction) => Map[LocalVar, LocalVar]()
      case (p:SSAPhiInstruction,c:SSAPhiInstruction) => Map[LocalVar, LocalVar](LocalVar(p.getDef) -> LocalVar(c.getDef), LocalVar(p.getUse(0)) -> LocalVar(c.getUse(0)),LocalVar(p.getUse(1)) -> LocalVar(c.getUse(1)))
      case (p:SSAPutInstruction,c:SSAPutInstruction) => Map[LocalVar, LocalVar]()
      case (p:SSAGetInstruction,c:SSAGetInstruction) => Map[LocalVar, LocalVar]()
      case (p:SSAGotoInstruction,c:SSAGotoInstruction) => Map[LocalVar, LocalVar]()
      case (p:SSABinaryOpInstruction,c:SSABinaryOpInstruction) => Map[LocalVar, LocalVar](LocalVar(p.getDef) -> LocalVar(c.getDef), LocalVar(p.getUse(0)) -> LocalVar(c.getUse(0)),LocalVar(p.getUse(1)) -> LocalVar(c.getUse(1)))
      case (p:SSAConditionalBranchInstruction,c:SSAConditionalBranchInstruction) => Map[LocalVar, LocalVar](LocalVar(p.getUse(0)) -> LocalVar(c.getUse(0)),LocalVar(p.getUse(1)) -> LocalVar(c.getUse(1)))
      case (p:SSAArrayStoreInstruction,c:SSAArrayStoreInstruction) =>  Map[LocalVar, LocalVar](LocalVar(p.getDef) -> LocalVar(c.getDef), LocalVar(p.getArrayRef) -> LocalVar(c.getArrayRef))
      case (p:SSAArrayLoadInstruction,c:SSAArrayLoadInstruction) =>  Map[LocalVar, LocalVar](LocalVar(p.getDef) -> LocalVar(c.getDef), LocalVar(p.getArrayRef) -> LocalVar(c.getArrayRef))
      case (p:SSANewInstruction,c:SSANewInstruction) => Map[LocalVar, LocalVar](LocalVar(p.getDef) -> LocalVar(c.getDef))
      case (_,_)=> throw new Exception("Unknown Instruction")
    }
  }

  def approxDifference():(List[SSAInstruction],List[SSAInstruction],Map[LocalVar,LocalVar],Map[String,SSAInstruction]) = {
    val previous = getOldInstructions
    val current = getInstructions
    var deletions = new ListBuffer[SSAInstruction]()
    var additions = new ListBuffer[SSAInstruction]()
    var transformMap = Map[LocalVar, LocalVar]()
    var transformInstructions = Map[String,SSAInstruction]()
    var i=0
    var j=0
    while (i < previous.length && j < current.length) {
      if(instrType(previous(i)) == instrType(current(j))) {
        transformMap = transformMap ++ transform(previous(i),current(j))
        transformInstructions = transformInstructions + (previous(i).toString -> current(j))
        i = i + 1
        j = j + 1
      }
      else if(j+1 == current.length) {
        deletions += previous(i)
        i = i + 1
      }
      else if(i+1 == previous.length) {
        additions += current(j)
        j = j + 1
      }
      else if(instrType(previous(i)) == instrType(current(j+1))) {
        additions += current(j)
        j = j + 1
      }
      else if(instrType(previous(i+1)) == instrType(current(j))) {
        deletions += previous(i)
        i = i + 1
      }
      else {
        deletions += previous(i)
        additions += current(j)
        i = i + 1
        j = j + 1
      }
    }
    while(i < previous.length) {
      deletions += previous(i)
      i = i + 1
    }
    while(j < current.length) {
      additions += current(j)
      j = j + 1
    }
    (deletions.toList,additions.toList,transformMap,transformInstructions)
  }

  def processOld(init: State) : State = {
    var last = init
    var next = stepOld(last)
    var counter = 1
    while (next.worklist.nonEmpty && (next.relevants.toSet diff last.relevants.toSet).toMap.nonEmpty ) {
      last = next
      next = stepOld(next)
      counter += 1
    }
    println("No of Iterations to reach fixPoint again:" + counter)
    next
  }

  def process(init: State) : State = {
    var last = init
    var next = step(last)
    var counter = 1
    while (next.worklist.nonEmpty && (next.relevants.toSet diff last.relevants.toSet).toMap.nonEmpty ) {
      last = next
      next = step(next)
      counter += 1
    }
    println("No of Iterations to reach fixPoint again:" + counter)
    next
  }
  def execute(st:State): State = {

    val (deletions,additions,transformMap,transformInstructions) = approxDifference()
    println("Deleted Instructions:")
    for(instr <- deletions) {
      println(instr)
    }
    println("Added Instructions:")
    for(instr <- additions) {
      println(instr)
    }
    var state: State = st
    for (instr <- deletions) {
        val curInstr =  instr
        val nextInstr = getOldNextInstr(curInstr)
        val previousInstr = getOldPreviousInstr(curInstr)
        println("processing deletion " + curInstr)
        deleted += curInstr
        state = processOld(State(List(nextInstr),previousInstr ,state.history,state.slice - curInstr.toString,state.statics, state.locals,state.heap,state.refs - curInstr.toString,state.defs - curInstr.toString,state.relevants - curInstr.toString))
      }
    state = applyTransformations(state,transformMap,transformInstructions)
    state = applyAdditions(state,additions)
    for (instr <- additions) {
      val curInstr =  instr
      val previousInstr = getPreviousInstr(curInstr)
      println("processing addition " + curInstr)
      state = process(State(List(instr),previousInstr ,state.history,state.slice,state.statics, state.locals,state.heap,state.refs,state.defs,state.relevants ))
    }
    state
    }

  def applyAdditions(state:State,instructions:List[SSAInstruction]): State = {
    var refs = state.refs
    var defs = state.defs
    var relevants = state.relevants
    for (instruction <- instructions) {
      val (r,d) = instruction match {
      case instr: SSAReturnInstruction => (Set[LocalVar](),Set[LocalVar]())
      case instr: SSAPhiInstruction => (Set[LocalVar](LocalVar(instr.getUse(0)),LocalVar(instr.getUse(1))),Set[LocalVar](LocalVar(instr.getDef)))
      case instr: SSAPutInstruction => (Set[LocalVar](),Set[LocalVar]())
      case instr: SSAGetInstruction => (Set[LocalVar](),Set[LocalVar]())
      case instr: SSAGotoInstruction => (Set[LocalVar](),Set[LocalVar]())
      case instr: SSABinaryOpInstruction => (Set[LocalVar](LocalVar(instr.getUse(0)),LocalVar(instr.getUse(1))),Set[LocalVar](LocalVar(instr.getDef)))
      case instr: SSAConditionalBranchInstruction => (Set[LocalVar](LocalVar(instr.getUse(0)),LocalVar(instr.getUse(1))),Set[LocalVar]())
      case instr: SSAArrayStoreInstruction => (Set[LocalVar](LocalVar(instr.getValue)),Set[LocalVar](LocalVar(instr.getArrayRef)))
      case instr: SSAArrayLoadInstruction => (Set[LocalVar](LocalVar(instr.getArrayRef)),Set[LocalVar](LocalVar(instr.getDef)))
      case instr: SSANewInstruction =>  (Set[LocalVar](),Set[LocalVar](LocalVar(instr.getDef)))
      case _=> throw new Exception("Unknown Instruction")
    }
      refs +=  instruction.toString ->(instruction, r)
      defs +=  instruction.toString ->(instruction, d)
      relevants +=  instruction.toString ->(instruction, Set[LocalVar]())
    }
    State(state.worklist,state.lastInstr ,state.history,state.slice,state.statics, state.locals,state.heap,refs,defs,relevants)
  }

  def applyTransformations(state:State,transformMap:Map[LocalVar,LocalVar],transformInstructions:Map[String,SSAInstruction]):State = {
    var refs = Map[String, (SSAInstruction, Set[LocalVar])]()
    var defs = Map[String, (SSAInstruction, Set[LocalVar])]()
    var relevants = Map[String, (SSAInstruction, Set[LocalVar])]()
    var slice: Set[String] = Set[String]()
    state.defs.foreach {
      case (str, (instr, list)) =>
        val newList: Set[LocalVar] = applyTransformationtoList(transformMap,list)
        if (transformInstructions.contains(str)) {
          defs += (transformInstructions(str).toString -> (transformInstructions(str),newList))
        } else {
          defs += (str -> (instr,newList))
        }
    }
    state.refs.foreach {
      case (str, (instr, list)) =>
        val newList: Set[LocalVar] = applyTransformationtoList(transformMap,list)
        if (transformInstructions.contains(str)) {
          refs += (transformInstructions(str).toString -> (transformInstructions(str),newList))
        } else {
          refs += (str -> (instr,newList))
        }
    }
    state.relevants.foreach {
      case (str, (instr, list)) =>
        val newList: Set[LocalVar] = applyTransformationtoList(transformMap,list)
        if (transformInstructions.contains(str)) {
          relevants += (transformInstructions(str).toString -> (transformInstructions(str),newList))
        } else {
          relevants += (str -> (instr,newList))
        }
    }
    for (l <- state.slice) {
      if (transformInstructions.contains(l)) {
        slice += transformInstructions(l).toString
      } else {
        slice += l
      }
    }
    State(state.worklist,state.lastInstr ,state.history,slice,state.statics, state.locals,state.heap,refs,defs,relevants)
  }
  def applyTransformationtoList(transformMap:Map[LocalVar,LocalVar],list:Set[LocalVar]):Set[LocalVar] = {
    var newList: Set[LocalVar] = Set[LocalVar]()
    for (l <- list) {
      if (transformMap.contains(l)) {
        newList += transformMap(l)
      } else {
        newList += l
      }
    }
    newList
  }
  def print(state: State): Unit = {
    println("Slice:" + state.slice)
    println("Defs")
    state.defs.foreach {
      case (str, (instr, list)) =>
        if (list.nonEmpty) {
          println(str + "=> " + list)
        }
    }
    println("Refs")
    state.refs.foreach {
      case (str, (instr, list)) =>
        if (list.nonEmpty) {
          println(str + "=> " + list)
        }
    }
    println("Relevants")
    state.relevants.foreach {
      case (str, (instr, list)) =>
        if (list.nonEmpty) {
          println(str + "=> " + list)
        }
    }
  }
}