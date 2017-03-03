package com.ideal.evecore.universe.execution

import java.util.{Date, Timer, TimerTask}

import com.ideal.evecore.interpreter._
import com.ideal.evecore.universe.World
import com.ideal.evecore.universe.execution.TaskHandler.ExecutionProcessor
import com.ideal.evecore.universe.receiver.EveObjectMessage
import com.rokuan.calliopecore.sentence.IAction

import scala.util.{Failure, Success, Try}

/**
  * Created by Christophe on 28/12/2016.
  */
class TaskHandler(private val world: World) {
  import EveResultObject._
  val timer = new Timer()

  /*def scheduleDelayedTask(time: EveObject, o: ObjectValueSource): EveResultObject = {
    val executeFunc: ExecutionProcessor[ObjectValueSource] = execute
    time match {
      case EveDateObject(d) => scheduleFixedDateTask(d, o, execute)
      case EveTimeObject(t) => new EveFailureObject(new Exception("Not implemented yet")) // TODO: create an event-based system around ITimeObject
      case EveObjectList(Seq(EveDateObject(from), EveDateObject(to))) => scheduleDurationTask(from, to, o, execute)
      case _ => Ok
    }
  }*/

  def scheduleDelayedTask(time: EveObject, o: EveStructuredObject): EveResultObject = {
    val executeFunc: ExecutionProcessor[EveStructuredObject] = execute
    time match {
      case EveDateObject(d) => scheduleFixedDateTask(d, o, execute)
      case EveTimeObject(t) => new EveFailureObject(new Exception("Not implemented yet")) // TODO: create an event-based system around ITimeObject
      case EveObjectList(Seq(EveDateObject(from), EveDateObject(to))) => scheduleDurationTask(from, to, o, execute)
      case _ => Ok
    }
  }

  def scheduleDelayedStateTask(time: EveObject, action: IAction, what: EveObject): EveResultObject = {
    val o: EveStructuredObject = what
    val executeFunc: EveStructuredObject => EveResultObject = (o: EveStructuredObject) => {
      Try(o.setState(action.getBoundState, action.getState)) match {
        case Success(_) => Ok
        case Failure(e) => Ko(e)
      }
    }

    time match {
      case EveDateObject(d) => scheduleFixedDateTask(d, o, executeFunc)
      case EveTimeObject(t) => Ko(new Exception("Not implemented yet")) // TODO: create an event-based system around ITimeObject
      case EveObjectList(Seq(EveDateObject(from), EveDateObject(to))) => scheduleDurationTask(from, to, o, executeFunc)
      case _ => Ok
    }
  }

  def scheduleFixedDateTask[T](d: Date, o: T, exec: ExecutionProcessor[T]): EveResultObject = {
    if(!d.after(new Date())){
       exec(o)
    } else {
      val task = new TimerTask {
        override def run(): Unit = exec(o)
      }
      timer.schedule(task, d)
      Ok
    }
  }

  def scheduleDurationTask[T](from: Date, to: Date, o: T, exec: ExecutionProcessor[T]): EveResultObject = {
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
    Ok
  }

  /*private def execute(o: ObjectValueSource): EveResultObject = world.findReceiver(o)
    .map(_.handleMessage(ObjectMessage(o)): EveResultObject)
    .getOrElse(Ok)*/

  private def execute(o: EveStructuredObject): EveResultObject = world.findReceiver(o)
    .map(_.handleMessage(EveObjectMessage(o)): EveResultObject)
    .getOrElse(Ok)
}

object TaskHandler {
  type ExecutionProcessor[T] = T => EveResultObject
}
