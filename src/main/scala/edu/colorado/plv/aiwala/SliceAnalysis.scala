package edu.colorado.plv.aiwala

import com.ibm.wala.ssa.IR
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa._
import com.ibm.wala.types.FieldReference
import scala.collection.JavaConversions._
import com.ibm.wala.types.MethodReference
import com.ibm.wala.shrikeBT.IBinaryOpInstruction
import com.ibm.wala.shrikeBT.IConditionalBranchInstruction

sealed case class LocalVar(index : Int)
sealed case class StaticVar(fld : FieldReference)
sealed case class Val[T](value : T)

case class State(worklist: List[SSAInstruction],
                 lastInstr: SSAInstruction,
                 history : List[SSACFG#BasicBlock],
                 slice : Set[String],
                 statics : Map[StaticVar,Val[_]],
                 locals : Map[LocalVar, Val[_]],
                 heap : Map[(LocalVar,LocalVar),Val[_]],
                 refs: Map[String, (SSAInstruction, Set[LocalVar])],
                 defs: Map[String, (SSAInstruction, Set[LocalVar])],
                 relevants: Map[String, (SSAInstruction, Set[LocalVar])])

class SliceAnalysis(ir: IR,seed:Int){
  var refs = Map[String, (SSAInstruction, Set[LocalVar])]()
  var defs = Map[String, (SSAInstruction, Set[LocalVar])]()
  var relevants = Map[String, (SSAInstruction, Set[LocalVar])]()

  lazy val initialState: State = {
    val history: List[SSACFG#BasicBlock] = List[SSACFG#BasicBlock](ir.getControlFlowGraph.getBasicBlock(0))
    val instrs = getInstructions
    for(i <- 0 until instrs.length) {
     val (r,d) = instrs(i) match {
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
      refs +=  instrs(i).toString ->(instrs(i), r)
      defs +=  instrs(i).toString ->(instrs(i), d)
      relevants +=  instrs(i).toString ->(instrs(i), Set[LocalVar]())
    }
    val slice: Set[String] = Set[String](instrs(0).toString)
    val source : Set[LocalVar] = Set[LocalVar](LocalVar(seed))
    val (curRelevance,curSlice) = calculateRelevance(instrs(0),refs,defs,source)
    State(instrs(0) :: Nil,instrs(0),history,slice ++ curSlice, Map(): Map[StaticVar, Val[_]], Map(): Map[LocalVar, Val[_]],Map(): Map[(LocalVar,LocalVar), Val[_]],refs,defs,relevants.updated(instrs(0).toString, (instrs(0), curRelevance)))
  }

  lazy val getInstructions : Array[SSAInstruction] = {
    ir.getControlFlowGraph.flatMap {bb => bb.iterator}.toArray
  }

  lazy val nextInstrMap : Map[Int,SSAInstruction] = {
    val instrs : Array[SSAInstruction] = getInstructions
    (0 to instrs.length - 2).map {i => (instrs(i).hashCode,instrs(i+1))}.toMap
  }

  def getNextInstr(instr: SSAInstruction): SSAInstruction = {
    nextInstrMap.get(instr.hashCode).get
  }

  def getInstrIndex(instruction:SSAInstruction): Int = {
    val instrs = ir.getInstructions()
    for (i <- 0 until instrs.length) {
      if (instruction == instrs(i)) {
        return i;
      }
    }
    return getPiInstrIndex(instruction);
  }

  def getPiInstrIndex(instruction:SSAInstruction): Int = {
    val basicBlock: ISSABasicBlock = ir.getBasicBlockForInstruction(instruction)
    return basicBlock.getFirstInstructionIndex;
  }

  def getInstrAtIndex(index: Int): SSAInstruction = {
    val targetBlock = ir.getControlFlowGraph.find { bb => bb.getFirstInstructionIndex >= index && bb.iterator.hasNext}
    targetBlock match {
      case Some(bb) => bb.iterator.next
      case None => throw new Exception("Instruction not found")
    }
  }

  def printInstructions() : Unit = {
    val instrs = getInstructions
    for(i <- 0 until instrs.length) {
      println(i + " => " + instrs(i) + " => " + getInstrIndex(instrs(i)))
    }
    }

  def getValueString(valueNumber: Int, symbolTable: SymbolTable): String = {
    if (symbolTable == null) {
      Integer.toString(valueNumber)
    } else {
      val ret = symbolTable.getValueString(valueNumber)
      if (ret.contains(":")) {
        ret.split("#")(1)
      } else {
        ret
      }
    }
  }

  def getLocals(i: Int, locals: Map[LocalVar, Val[_]]): (Val[_], Map[LocalVar, Val[_]]) = {
      locals.get(LocalVar(i)) match {
        case Some(x) => (x, locals)
        case None =>
          val res = ir.getSymbolTable.getConstantValue(i)
          val newVal: Val[_] = Val(res)
          (newVal, locals.updated(LocalVar(i), newVal))
      }
  }

  def getFromHeap(i: Int,j: Int,heap: Map[(LocalVar,LocalVar), Val[_]]): (Val[_]) = {
    heap.get((LocalVar(i),(LocalVar(j)))) match {
      case Some(x) => x
      case None => throw new Exception("Heap reference exception")
    }
  }

  def setToHeap(i: Int,j: Int,value: Val[_],heap: Map[(LocalVar,LocalVar), Val[_]]): Map[(LocalVar,LocalVar), Val[_]] = {
    (heap.updated((LocalVar(i),(LocalVar(j))), value))
  }

  def getStatic(fld: FieldReference, statics: Map[StaticVar,  Val[_]]): (Val[_], Map[StaticVar,  Val[_]]) = {
      statics.get(StaticVar(fld)) match {
        case Some(x) => (x, statics)
        case None =>
          val newVal:  Val[_] = Val(0)
          (newVal, statics.updated(StaticVar(fld), newVal))
      }
  }

  def Conditional(val1: Val[_], val2: Val[_], op: IConditionalBranchInstruction.Operator): Boolean = {
    (val1, val2) match {
      case (l: Val[_], r: Val[_]) =>
        op match {
          case IConditionalBranchInstruction.Operator.EQ => if (l.value == r.value) true else false
          case IConditionalBranchInstruction.Operator.GE => if (l.value.asInstanceOf[Int] >= r.value.asInstanceOf[Int]) true else false
          case IConditionalBranchInstruction.Operator.GT => if (l.value.asInstanceOf[Int] > r.value.asInstanceOf[Int])true else false
          case IConditionalBranchInstruction.Operator.LE => if (l.value.asInstanceOf[Int] <= r.value.asInstanceOf[Int]) true else false
          case IConditionalBranchInstruction.Operator.LT => if (l.value.asInstanceOf[Int] < r.value.asInstanceOf[Int]) true else false
          case IConditionalBranchInstruction.Operator.NE => if (l.value.asInstanceOf[Int] != r.value.asInstanceOf[Int]) true else false
        }
      case (_, _) => throw new Exception("Not implemented")
    }
  }

  def BinaryOperation(val1: Val[_], val2: Val[_], op: IBinaryOpInstruction.Operator): Val[_] = {
    (val1, val2) match {
      case (l: Val[_], r: Val[_]) =>
        op match {
          case IBinaryOpInstruction.Operator.ADD => Val(l.value.asInstanceOf[Int] + r.value.asInstanceOf[Int])
          case IBinaryOpInstruction.Operator.SUB => Val(l.value.asInstanceOf[Int] - r.value.asInstanceOf[Int])
          case IBinaryOpInstruction.Operator.MUL => Val(l.value.asInstanceOf[Int] * r.value.asInstanceOf[Int])
          case IBinaryOpInstruction.Operator.DIV => Val(l.value.asInstanceOf[Int] / r.value.asInstanceOf[Int])
          case IBinaryOpInstruction.Operator.OR => Val(l.value.asInstanceOf[Boolean] || r.value.asInstanceOf[Boolean])
          case IBinaryOpInstruction.Operator.AND => Val(l.value.asInstanceOf[Boolean] && r.value.asInstanceOf[Boolean])
          case _ =>  throw new Exception("Not implemented")
        }
      case (_, _) => throw new Exception("Not implemented")
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

  def lookUp(basicBlock:SSACFG#BasicBlock,a:Int,b:Int,statics : Map[StaticVar,Val[_]],
             locals : Map[LocalVar, Val[_]]): (Val[_], Map[LocalVar, Val[_]]) = {
    val instrs = basicBlock.getAllInstructions()
    for (instr <- instrs) {
      if (instr.getDef == a ) {
        return getLocals(instr.getDef, locals)
      }
    }
    for (instr <- instrs) {
      if (instr.getDef == b ) {
        return getLocals(instr.getDef, locals)
      }
    }
    return (Val(0),locals)
  }

  def step (state: State): State = {
    val list = state.worklist
    val slice = state.slice
    val curInstr = list.head
    val lastInstr = state.lastInstr
    val relevants = state.relevants
    val (curRelevance,curSlice) = calculateRelevance(curInstr,state.refs,state.defs,relevants.get(lastInstr.toString).get._2)
    val history = state.history
    val statics = state.statics
    val locals = state.locals
    val heap = state.heap
    if (list.isEmpty)
      return state
    curInstr match {
      case i: SSANewInstruction =>
        val currentBlock: SSACFG#BasicBlock = ir.getControlFlowGraph.getBlockForInstruction(getInstrIndex(i));
        if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
          State(getNextInstr(i) :: list.tail, i,history:+ currentBlock,slice ++ curSlice,statics, locals,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
        else {
          State(getNextInstr(i) :: list.tail, i,history,slice ++ curSlice,statics, locals,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
      case i: SSAArrayStoreInstruction =>
        val (value: Val[_], newLocal1) = getLocals(i.getValue, locals)
        val (index: Val[_], newLocal2) = getLocals(i.getIndex, newLocal1)
        val currentBlock: SSACFG#BasicBlock = ir.getControlFlowGraph.getBlockForInstruction(getInstrIndex(i));
        if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
          State(getNextInstr(i) :: list.tail, i,history:+ currentBlock,slice ++ curSlice,statics, newLocal2,setToHeap(i.getArrayRef,index.value.asInstanceOf[Int],value,heap),state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
        else {
          State(getNextInstr(i) :: list.tail, i,history,slice ++ curSlice,statics, newLocal2,setToHeap(i.getArrayRef,index.value.asInstanceOf[Int],value,heap),state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
      case i: SSAArrayLoadInstruction =>
        val localVar: LocalVar = LocalVar(i.getDef)
        val (index: Val[_], newLocal) = getLocals(i.getIndex, locals)
        val value: Val[_] = getFromHeap(i.getArrayRef,index.value.asInstanceOf[Int],heap)
        val currentBlock: SSACFG#BasicBlock = ir.getControlFlowGraph.getBlockForInstruction(getInstrIndex(i));
        if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
          State(getNextInstr(i) :: list.tail, i,history:+ currentBlock,slice ++ curSlice,statics, newLocal.updated(localVar, value),heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
        else {
          State(getNextInstr(i) :: list.tail, i,history,slice ++ curSlice,statics, newLocal.updated(localVar, value),heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
      case i: SSAReturnInstruction => State(list.tail, i,history,slice ++ curSlice,statics, locals,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
      case i: SSAPhiInstruction =>
        val localVar: LocalVar = LocalVar(i.getDef)
        val predNodes = ir.getControlFlowGraph.getPredNodes(ir.getBasicBlockForInstruction(i))
        var j = 0
        while (predNodes.hasNext) {
          val bb = predNodes.next()
          if (bb == history.last) {
            while (predNodes.hasNext)
              predNodes.next()
          } else {
            j += 1
          }
        }
        if(j<i.getNumberOfUses) {
          val (value, newLocal) = getLocals(i.getUse(j), locals)
          State(getNextInstr(i) :: list.tail,i,history,slice ++ curSlice,
            statics,
            newLocal.updated(localVar, value),heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
        else {
          val (value : Val[_],newLocals) = lookUp(history.last,i.getUse(0),i.getUse(1),statics,locals)
          State(getNextInstr(i) :: list.tail,i,history,slice ++ curSlice,
            statics,
            newLocals.updated(localVar, value),heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }

      case i: SSAPutInstruction =>
        val staticVar: StaticVar = StaticVar(i.getDeclaredField())
        val (value: Val[_], newLocals) = getLocals(i.getUse(0), locals)
        val currentBlock: SSACFG#BasicBlock = ir.getControlFlowGraph.getBlockForInstruction(getInstrIndex(i));
        if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
              State(
                getNextInstr(i) :: list.tail,i,history :+ currentBlock,slice ++ curSlice,
                statics.updated(staticVar, value),
                newLocals,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
        else {
              State(
                getNextInstr(i) :: list.tail,i,history,slice ++ curSlice,
                statics.updated(staticVar, value),
                newLocals,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }

      case i: SSAGetInstruction =>
        val localVar: LocalVar = LocalVar(i.getDef)
        val (value: Val[_], newStatics) = getStatic(i.getDeclaredField, statics)
        val currentBlock: SSACFG#BasicBlock = ir.getControlFlowGraph.getBlockForInstruction(getInstrIndex(i));
        if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
              State(
                getNextInstr(i) :: list.tail,i,history :+ currentBlock,slice ++ curSlice,
                newStatics,
                locals.updated(localVar, value),heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
        else {
              State(
                getNextInstr(i) :: list.tail,i,history,slice ++ curSlice,
                newStatics,
                locals.updated(localVar, value),heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }

      case i: SSAGotoInstruction =>
        val currentBlock: SSACFG#BasicBlock = ir.getControlFlowGraph.getBlockForInstruction(getInstrIndex(i));
        if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
          State(getInstrAtIndex(i.getTarget()) :: list.tail,i,history :+ currentBlock, slice ++ curSlice,
            statics, locals,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }
        else {
          State(getInstrAtIndex(i.getTarget()) :: list.tail,i,history, slice ++ curSlice,
            statics, locals,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }

      case i: SSABinaryOpInstruction =>
        val operator: IBinaryOpInstruction.Operator = i.getOperator.asInstanceOf[IBinaryOpInstruction.Operator]
        val (val1: Val[_], newLocal1) = getLocals(i.getUse(0), locals)
        val (val2: Val[_], newLocal2) = getLocals(i.getUse(1), newLocal1)
        val newVal = BinaryOperation(val1, val2, operator)
        val currentBlock: SSACFG#BasicBlock = ir.getControlFlowGraph.getBlockForInstruction(getInstrIndex(i));
        if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
              State(
                getNextInstr(i) :: list.tail,i,history :+ currentBlock,slice ++ curSlice,
                statics,
                locals.updated(LocalVar(i.getDef), newVal),heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        } else {
              State(
                getNextInstr(i) :: list.tail,i,history,slice ++ curSlice,
                statics,
                locals.updated(LocalVar(i.getDef), newVal),heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
        }

      case i: SSAConditionalBranchInstruction =>
        val operator: IConditionalBranchInstruction.Operator = i.getOperator.asInstanceOf[IConditionalBranchInstruction.Operator]
        val (v1: Val[_], newLocal1) = getLocals(i.getUse(0), locals)
        val (v2: Val[_], newLocal2) = getLocals(i.getUse(1), newLocal1)
        val newVal = Conditional(v1, v2, operator)
        val currentBlock: SSACFG#BasicBlock = ir.getControlFlowGraph.getBlockForInstruction(getInstrIndex(i));
        newVal match {
          case b: Boolean =>
            if (b) {
              if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
                State(getInstrAtIndex(i.getTarget()) :: list.tail,i, history :+ currentBlock, slice ++ curSlice,
                  statics, newLocal2,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
              }
              else {
                State(getInstrAtIndex(i.getTarget()) :: list.tail,i, history, slice ++ curSlice,
                  statics, newLocal2,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
              }
            }
            else {
              if (getInstrIndex(i) == currentBlock.getLastInstructionIndex()) {
                State(getNextInstr(i) :: list.tail,i, history :+ currentBlock, slice ++ curSlice,
                  statics, newLocal2,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
              }
              else {
                State(getNextInstr(i) :: list.tail,i, history, slice ++ curSlice,
                  statics, newLocal2,heap,state.refs,state.defs,state.relevants.updated(curInstr.toString, (curInstr, curRelevance)))
              }
            }
        }
    }
  }

  def execute(init: State) : State = {
    var last = init
    var next = step(last)
    var counter = 1
    do {
      last = next
      next = step(next)
      counter += 1
    }while (next.worklist.nonEmpty && (next.relevants.toSet diff last.relevants.toSet).toMap.nonEmpty )
    println("No of Iterations to reach fixPoint:" + counter)
    next
  }

  def print(state: State): Unit = {
    println("Slice: " + state.slice)
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
  def equalMaps(m1:collection.mutable.Map[LocalVar, Val[_]] , m2:collection.mutable.Map[LocalVar, Val[_]]) : Boolean = {
    if (m1.size != m2.size)
      return false;
    for (key <- m1.keys) {
      if (!m1.get(key).equals(m2.get(key)))
        return false;
    }
    return true;
  }
  def printSet(args: scala.collection.mutable.Set[_]): Unit = {
    args.foreach(println)
  }
  def extractVariable(x: SSAInstruction): LocalVar = x match {
    case x : SSAPhiInstruction => LocalVar(x.getDef)
    case x : SSABinaryOpInstruction => LocalVar(x.getDef)
    case _ => null
  }
}