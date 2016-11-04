package com.ideal.evecore.universe.execution

import java.util.{TimerTask, Date, Timer}

import com.ideal.evecore.interpreter.{EveObjectList, EveTimeObject, EveDateObject, EveObject}
import com.ideal.evecore.universe.World
import com.ideal.evecore.universe.receiver.ObjectMessage
import com.ideal.evecore.universe.route.ObjectValueSource

/**
 * Created by chris on 03/11/2016.
 */
object TaskPool {
  val timer = new Timer()

  def scheduleDelayedTask(time: EveObject, o: ObjectValueSource) = {
    time match {
      case EveDateObject(d) => scheduleFixedDateTask(d, o)
      case EveTimeObject(t) => // TODO: create an event-based system around ITimeObject
      case EveObjectList(Seq(EveDateObject(from), EveDateObject(to))) => scheduleDurationTask(from, to, o)
      case _ =>
    }
  }

  def scheduleFixedDateTask(d: Date, o: ObjectValueSource) = {
    val task = new TimerTask {
      override def run(): Unit = execute(o)
    }
    timer.schedule(task, d)
  }

  def scheduleDurationTask(from: Date, to: Date, o: ObjectValueSource) = {
    if(from.before(to)) {
      val startTask = new TimerTask {
        override def run(): Unit = execute(o)
      }
      val stopTask = new TimerTask {
        override def run(): Unit = startTask.cancel()
      }
      timer.schedule(startTask, from)
      timer.schedule(stopTask, to)
    }
  }

  private def execute(o: ObjectValueSource) = World.findReceiver(o).map(_.handleMessage(ObjectMessage(o)))
}
