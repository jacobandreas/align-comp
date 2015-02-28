package task

import java.io.File

import framework.fodor.graph.EventContext
import framework.igor.eval.EvalStats
import main.Config

/**
 * @author jda
 */
trait TaskFactory {
  def apply(dataRoot: File)(implicit config: Config): Task
}

trait Task {
  type State <: TaskState
  type Action <: TaskAction
  type Instance = TaskInstance[State,Action]

  def instances: IndexedSeq[Instance]
  def trainIds: IndexedSeq[Int]
  def testIds: IndexedSeq[Int]

  def availableActions(state: State): Iterable[Action]
  def doAction(state: State, action: Action): State

  def visualize(pred: IndexedSeq[(State,Action,State)], gold: IndexedSeq[(State,Action,State)]): Unit
  def score(pred: IndexedSeq[(State,Action,State)], gold: IndexedSeq[(State,Action,State)]): EvalStats

  def represent(s1: State, a: Action, s2: State): EventContext
}

trait TaskState
trait TaskAction

case class TaskInstance[+S,+A](path: IndexedSeq[(S,A,S)], instructions: IndexedSeq[String])
