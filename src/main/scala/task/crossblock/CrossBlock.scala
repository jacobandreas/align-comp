package task.crossblock

import java.io.File
import java.util.Properties

import breeze.stats.distributions.Rand
import framework.fodor.{Feature, IndicatorFeature, SimpleFeature}
import framework.fodor.graph.{GraphWorld, Event, EventContext}
import framework.igor.eval.EvalStats
import main.Config
import task._

import scala.collection.mutable
import scala.io.Source

/**
 * @author jda
 */
object CrossBlock extends TaskFactory {
  override def apply(dataRoot: File)(implicit config: Config): Task = new CrossBlock(new File(dataRoot, "crossblock"))
}

class CrossBlock(root: File)(implicit config: Config) extends Task with Serializable {

  val dataRoot = new File(root, "data")

  override type State = CrossBlockState
  override type Action = CrossBlockAction

  override def instances: IndexedSeq[Instance] = {

    val paired = starts zip collapsedAnnotations
    assert (paired.length == 49)
    val sequences = paired map { case (inst, annSeq) =>
      if (annSeq.isEmpty)
        IndexedSeq()
      else {
        val init = (inst, annSeq.head, doAction(inst, annSeq.head))
        val tripleSeq = annSeq.tail.foldLeft(IndexedSeq(init)) { (soFar, action) =>
          val lastState = soFar.last._3
          val nextTriple = (lastState, action, doAction(lastState, action))
          soFar :+ nextTriple
        }
        assert (tripleSeq.last._3.isWin)
        tripleSeq
      }
    }

    val insts = instructions zip sequences map {
      case (null, path) => null
      case (IndexedSeq(), path) => null
      case (inst, path) => TaskInstance(path, inst)
    } filterNot (_ == null)
    assert (insts.length == 46)
    insts
  }

  def starts: IndexedSeq[State] = {
    val gameFile = new File(dataRoot, "games.csv")
    val gameSource = Source.fromFile(gameFile)
    val instanceBuilder = IndexedSeq.newBuilder[State]
    val lineBuffer = mutable.Buffer[String]()
    gameSource.getLines().foreach { line =>
      if (line == "") {
        val lines = lineBuffer.toIndexedSeq
        lineBuffer.clear()
        val canStrike = lines(1).drop(1).toInt
        val board = lines.drop(2).map { boardLine =>
          val cells = boardLine.split(" ")
          cells.map {
            case "#" => true
            case "." => false
          }.toIndexedSeq
        }
        instanceBuilder += new CrossBlockState(canStrike, board, this)
      } else {
        lineBuffer += line
      }
    }
    instanceBuilder.result()
  }

  def instructions: IndexedSeq[IndexedSeq[String]] = {
    val instFile = new File(dataRoot, "instructions.bgm")
    val instSource = Source.fromFile(instFile)
    val pattern = """.*[a-z]+.*"""
    val bigramLines = instSource.getLines().filter(_ matches pattern)
    val cleanToks = bigramLines.map(_.split(" ").filterNot(_.contains("_")).mkString(" ")).toIndexedSeq
    val separated = cleanToks.map(_.split(" , | then ").toIndexedSeq).toIndexedSeq
    val numPattern = """# \d+"""
    val numberLines = instSource.reset().getLines().filter((_ matches numPattern))
    val numbers = numberLines.map(_.drop(2).toInt).toIndexedSeq
    assert (separated.length == numbers.length)
    val paired = numbers zip separated
    IndexedSeq.tabulate(paired.last._1+1) { i =>
      paired.find(_._1 - 1 == i).map(_._2).orNull
    }
  }

  val annotations: IndexedSeq[IndexedSeq[Action]] = {
    val partialRow = """r:(\d+):(\d+):(\d+)""".r
    val partialCol = """c:(\d+):(\d+):(\d+)""".r
    val fullRow = """r:(\d+)""".r
    val fullCol = """c:(\d+)""".r

    val theStarts = starts
    val annFile = new File(dataRoot, "annotations.txt")
    val annSource = Source.fromFile(annFile)
    val actionBuilder = IndexedSeq.newBuilder[IndexedSeq[Action]]
    val lineBuffer = mutable.Buffer[String]()
    annSource.getLines().foreach { line =>
      if (line == "") {
        val lines = lineBuffer.toIndexedSeq
        val problemId = lines.head.toInt
        val start = theStarts(problemId-1)
        val actions = lines.tail.map {
          case partialRow(r, c1, c2) => PartialAction(Row, r.toInt, c1.toInt, c2.toInt)
          case partialCol(c, r1, r2) => PartialAction(Column, c.toInt, r1.toInt, r2.toInt)
          case fullRow(r) => SingleAction(Row, r.toInt, start.cols)
          case fullCol(c) => SingleAction(Column, c.toInt, start.rows)
        }.toIndexedSeq

        actionBuilder += actions
        lineBuffer.clear()
      } else {
        lineBuffer += line
      }
    }
    actionBuilder.result()
  }

  val collapsedAnnotations = (annotations zip starts) map { case (actions, start) =>

    val extendedActions = actions.foldLeft((Nil: List[Action], start)) { case ((soFar, state), action) =>
      val acts = availableActions(state).toSeq
      if (acts contains action)
        (action :: soFar, doAction(state, action))
      else {
        val pa = action.asInstanceOf[PartialAction]
        val extended = acts.find {
          case SingleAction(pa.orientation, pa.major, _) => true
          case _ => false
        }
        assert (extended.isDefined)
        (extended.get :: soFar, doAction(state, extended.get))
      }
    }._1.reverse.toIndexedSeq

    val collapsedActions = extendedActions.foldLeft(Nil: List[Action]) {
      case (SingleAction(o, major, size) :: rest, SingleAction(o2, major2, size2)) if o == o2 =>
        MultiAction(o, IndexedSeq(major, major2), size) :: rest
      case (MultiAction(o, majors, size) :: rest, SingleAction(o2, major2, size2)) if o == o2 =>
        MultiAction(o, majors :+ major2, size) :: rest
      case (soFar, next) => next :: soFar
    }.reverse.toIndexedSeq
    collapsedActions
  }

  val randIds = Rand.permutation(46).sample(10)(config.fold)
  override val trainIds: IndexedSeq[Int] = randIds take 36
  override val testIds: IndexedSeq[Int] = randIds drop 36 take 10

  override def availableActions(state: State): Iterable[Action] = {
    val board = state.board
    val rm = rowMoves(board, state.canStrike)
    val cm = colMoves(board, state.canStrike)
    rm ++ cm
  }

  def rowMoves(board: IndexedSeq[IndexedSeq[Boolean]], canStrike: Int): IndexedSeq[CrossBlockAction] = {
    val singleAndPartialActions = IndexedSeq.tabulate[Iterable[CrossBlockAction]](board.length) { row =>
      val inThisRow = board(row).count(t => t)
      val singleMoves = if (inThisRow == canStrike) Some(SingleAction(Row, row, board.head.length)) else None
      val partialMoves = {
        IndexedSeq.tabulate(board(row).length) { startCol =>
          IndexedSeq.tabulate(board(row).length - startCol) { wm1 =>
            val width = wm1 + 1
            val part = board(row).slice(startCol, startCol + width)
            val count = part.count(p => p)
            if (!board(row)(startCol) || !board(row)(startCol + width - 1)) None
            else if (count != canStrike) None
            else Some(PartialAction(Row, row, startCol, startCol + width - 1))
          }.flatten
        }.flatten
      }
//      singleMoves ++ partialMoves
      if (singleMoves.isDefined) singleMoves else partialMoves
    }.flatten
    val multiMajors = singleAndPartialActions.collect { case SingleAction(Row, major, size) => major }
    val multiAction = if (multiMajors.length > 1) Some(MultiAction(Row, multiMajors, board.head.length)) else None
    singleAndPartialActions ++ multiAction
  }

  def colMoves(board: IndexedSeq[IndexedSeq[Boolean]], canStrike: Int): IndexedSeq[CrossBlockAction] = {
    val singleAndPartialActions = IndexedSeq.tabulate[Iterable[CrossBlockAction]](board.head.length) { col =>
      val inThisCol = board.count(_(col))
      val singleMoves = if (inThisCol == canStrike) Some(SingleAction(Column, col, board.length)) else None
      val partialMoves = {
        IndexedSeq.tabulate(board.length) { startRow =>
          IndexedSeq.tabulate(board.length - startRow) { wm1 =>
            val width = wm1 + 1
            val part = board.slice(startRow, startRow + width).map(_(col))
            val count = part.count(p => p)
            if (!board(startRow)(col) || !board(startRow + width - 1)(col)) None
            else if (count != canStrike) None
            else Some(PartialAction(Column, col, startRow, startRow + width - 1))
          }.flatten
        }.flatten
      }
//      singleMoves ++ partialMoves
      if (singleMoves.isDefined) singleMoves else partialMoves
    }.flatten
    val multiMajors = singleAndPartialActions.collect { case SingleAction(Column, major, size) => major }
    val multiAction = if (multiMajors.length > 1) Some(MultiAction(Column, multiMajors, board.length)) else None
    singleAndPartialActions ++ multiAction
  }

  override def doAction(state: State, action: Action): State = {
//    val newBoard = state.board.map(_.clone())
//    var total = 0
//    action.cells.foreach { case (r, c) =>
//      if (newBoard(r)(c)) total += 1
//      newBoard(r)(c) = false
//    }
    var total = 0
    val newBoard = IndexedSeq.tabulate(state.rows) { r =>
      IndexedSeq.tabulate(state.cols) { c =>
        if (action.cells contains (r,c)) {
          if (state.board(r)(c)) total += 1
          false
        } else {
          state.board(r)(c)
        }
      }
    }
    assert (action match {
      case MultiAction(_, majors, _) => total == state.canStrike * majors.length
      case _ => total == state.canStrike
    })
    new CrossBlockState(state.canStrike, newBoard, this)
  }

  override def visualize(pred: IndexedSeq[(State, Action, State)], gold: IndexedSeq[(State, Action, State)]): Unit = Unit

  override def score(pred: IndexedSeq[(State, Action, State)], gold: IndexedSeq[(State, Action, State)]): EvalStats = {
//    // task completion scoring
//    if (pred.isEmpty) return EvalStats(0, 1, 0)
//    val last = pred.last._3
//    if (last.isWin) EvalStats(1, 0, 0)
//    else EvalStats(0, 1, 0)

//    // unordered exact match scoring
//    val predActions = pred.map(_._2).toSet
//    val goldActions = gold.map(_._2).toSet
//    val tp = (predActions & goldActions).size
//    val fp = (predActions diff goldActions).size
//    val fn = (goldActions diff predActions).size
//    if (fp == 0 && fn == 0) EvalStats(1, 0, 0) else EvalStats(0, 1, 0)

    // exact match scoring
    if (pred.map(_._2) == gold.map(_._2)) EvalStats(1, 0, 0) else EvalStats(0, 1, 0)
  }

  def collectActions(seq: IndexedSeq[(State,Action,State)]): Set[Set[(Int,Int)]] = {
    seq.map { case (s1, a, s2) =>
      a.cells.filter { case (r,c) => s1.board(r)(c) }.toSet
    }.toSet
  }

  override def represent(s1: State, a: Action, s2: State): EventContext = {
    val features: Set[Feature] = a match {
      case PartialAction(orientation, major, minorStart, minorEnd) =>
        Set(SimpleFeature("type=partial"), SimpleFeature(s"orientation=$orientation"))
      case SingleAction(orientation, major, size) =>
        Set(SimpleFeature("type=full"), SimpleFeature(s"orientation=$orientation"))
      case MultiAction(orientation, majors, size) =>
        Set(SimpleFeature("type=multi"), SimpleFeature(s"orientation=$orientation")) //, SimpleFeature(s"count=${majors.length}"))
    }
    val event = new Event(features + //SimpleFeature(s"canStrike=${s2.canStrike}"),
                          SimpleFeature(s"isLoss=${s2.isLoss}"))
    new EventContext(event,GraphWorld(Set()))
  }

//  def findSolution(initialState: State): IndexedSeq[(State, Action, State)] = {
//    val MaxDepth = 7
//    case class SearchState(gameState: State, depth: Int, parent: SearchState, parentAction: Action)
//    def unroll(s: SearchState): IndexedSeq[(State, Action, State)] = {
//      if (s.parent == null) IndexedSeq()
//      else unroll(s.parent) ++ IndexedSeq((s.gameState, s.parentAction, s.parent.gameState))
//    }
//
//    val queue = mutable.Queue[SearchState]()
//    queue.enqueue(SearchState(initialState, 0, null, null))
//    while (queue.nonEmpty) {
//      val currentState = queue.dequeue()
//      if (currentState.gameState.isWin) return unroll(currentState)
//      if (currentState.depth < MaxDepth) {
//        availableActions(currentState.gameState).foreach { action =>
//          val nextState = doAction(currentState.gameState, action)
//          queue.enqueue(SearchState(nextState, currentState.depth + 1, currentState, action))
//        }
//      }
//    }
//    //    System.exit(1)
//    IndexedSeq()
//  }

}

sealed trait Orientation
case object Row extends Orientation
case object Column extends Orientation

sealed trait CrossBlockAction extends TaskAction {
  val cells: IndexedSeq[(Int,Int)]
}

case class PartialAction(orientation: Orientation, major: Int, minorStart: Int, minorEnd: Int) extends CrossBlockAction {
  override val cells = orientation match {
    case Row => (minorStart to minorEnd) map { c => (major, c) }
    case Column => (minorStart to minorEnd) map { r => (r, major) }
  }
}

case class SingleAction(orientation: Orientation, major: Int, size: Int) extends CrossBlockAction {
  override val cells = orientation match {
    case Row => (0 until size) map { c => (major, c) }
    case Column => (0 until size) map { r => (r, major) }
  }
}

case class MultiAction(orientation: Orientation, majors: IndexedSeq[Int], size: Int) extends CrossBlockAction {
  override val cells = orientation match {
    case Row => majors flatMap { r => (0 until size) map { c => (r,c) } }
    case Column => majors flatMap { c => (0 until size) map { r => (r,c) } }
  }
}

case class CrossBlockState(canStrike: Int, board: IndexedSeq[IndexedSeq[Boolean]], task: CrossBlock) extends TaskState {
  val rows = board.length
  val cols = board.head.length
  val isWin = board.map(_.count(t => t)).sum == 0
  val isLoss = task.availableActions(this).isEmpty
}
