package com.ideal.evecore.evaluator

import java.util.{Date, Timer, TimerTask}

import com.ideal.evecore.evaluator.TaskHandler.ExecutionProcessor
import com.ideal.evecore.interpreter._
import com.ideal.evecore.interpreter.data.{EveObject, EveResultObject, EveStructuredObject}
import com.ideal.evecore.universe.World
import com.ideal.evecore.universe.receiver.{EveObjectMessage, Receiver}
import com.ideal.evecore.util.Result
import com.rokuan.calliopecore.sentence.IAction
import com.ideal.evecore.common.Conversions._

import scala.util.{Failure, Success, Try}

/**
 * Created by Christophe on 28/12/2016.
 */
class TaskHandler(private val world: World) {
  val timer = new Timer()

  /**
   * Executes a task at a certain time
   *
   * @param time The moment in time when the action should be executed
   * @param o The object containing all the needed data (action, source, target, time, ...)
   * @return The result of the operation (can be NoneObject is the action is not instant)
   */
  def scheduleDelayedTask(time: EObject, o: EStructuredObject): Result[EveObject] = {
    val executeFunc: ExecutionProcessor[EStructuredObject] = execute
    time match {
      case EDateObject(d) => scheduleFixedDateTask(d, o, execute)
      case ETimeObject(t) => EveResultObject.ko(new Exception("Not implemented yet")) // TODO: create an event-based system around ITimeObject
      case EObjectList(Seq(EDateObject(from), EDateObject(to))) => scheduleDurationTask(from, to, o, execute)
      case _ => EveResultObject.ok()
    }
  }

  /**
   * Executes a task at a certain time
   *
   * @param time The moment in time when the action should be executed
   * @param action The state-action to execute
   * @param what The object the action applies to
   * @return
   */
  def scheduleDelayedStateTask(time: EObject, action: IAction, what: EStructuredObject): Result[EveObject] = {
    val executeFunc: ExecutionProcessor[EStructuredObject] = (o: EStructuredObject) => {
      Try(o.setState(action.getBoundState, action.getState)) match {
        case Success(_) => EveResultObject.ok()
        case Failure(e) => EveResultObject.ko(e)
      }
    }

    time match {
      case EDateObject(d) => scheduleFixedDateTask(d, what, executeFunc)
      case ETimeObject(t) => EveResultObject.ko(new Exception("Not implemented yet")) // TODO: create an event-based system around ITimeObject
      case EObjectList(Seq(EDateObject(from), EDateObject(to))) => scheduleDurationTask(from, to, what, executeFunc)
      case _ => EveResultObject.ok()
    }
  }

  /**
   * Executes a task at a single fixed date in time
   * @param d The time to execute the task at
   * @param o The object to send to the receiver
   * @param exec The task execution method
   * @tparam T
   * @return
   */
  private def scheduleFixedDateTask[T](d: Date, o: T, exec: ExecutionProcessor[T]): Result[EveObject] = {
    if(!d.after(new Date())){
      exec(o)
    } else {
      val task = new TimerTask {
        override def run(): Unit = exec(o)
      }
      timer.schedule(task, d)
      EveResultObject.ok()
    }
  }

  /**
   * Executes a task during a certain perdio
   * @param from The start time
   * @param to The end time
   * @param o The object to handle
   * @param exec The task execution method
   * @tparam T
   * @return
   */
  private def scheduleDurationTask[T](from: Date, to: Date, o: T, exec: ExecutionProcessor[T]): Result[EveObject] = {
    if(from.before(to)) {
      val startTask = new TimerTask {
        override def run(): Unit = exec(o)
      }
      val stopTask = new TimerTask {
        override def run(): Unit = startTask.cancel()
      }
      timer.schedule(startTask, from)
      timer.schedule(stopTask, to)
    }
    EveResultObject.ok()
  }

  /**
   * Send this object to a receiver so it can handle it
   * @param o The object to process
   * @return The result of the execution by the matching receiver
   */
  private def execute(o: EStructuredObject): Result[EveObject] = {
    val content = implicitly[EveStructuredObject](o)
    world.findReceiver(content)
      .map((r: Receiver) => r.handleMessage(new EveObjectMessage(content)))
      .getOrElse(EveResultObject.ok())
  }
}

object TaskHandler {
  type ExecutionProcessor[T] = T => Result[EveObject]
}