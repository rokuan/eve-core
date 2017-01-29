package com.ideal.evecore.universe.execution

import java.util.{Date, Timer, TimerTask}

import com.ideal.evecore.interpreter._
import com.ideal.evecore.universe.World
import com.ideal.evecore.universe.receiver.ObjectMessage
import com.ideal.evecore.universe.route.ObjectValueSource

/**
  * Created by Christophe on 28/12/2016.
  */
class TaskHandler(private val world: World) {
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

  private def execute(o: ObjectValueSource) = world.findReceiver(o).map(_.handleMessage(ObjectMessage(o)))
}
