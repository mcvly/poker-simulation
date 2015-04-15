package org.mcvly.poker

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.mutable

import Poker._
import PokerSimulation._

/**
 * @author mcvly
 * @since 22.03.15
 */
object Actors {

  import Messages._

  object Messages {
    case class StartSimulation(numberOfWorkers: Int)
    case class Deal(n: Int)
    case class PlayerCombination(comb: Int)
    case class GatheredStatistics(combinations: Seq[Int], n: Int)
    case object Finished
  }

  class Manager extends Actor {

    val combinationCounts = mutable.ArraySeq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val statisticActor = context.actorOf(Props[Statistic], "statistic")
    var totalSimulations = 0
    var responsesReceived = 0
    var startTime = 0L

    def receive: Receive = {
      case StartSimulation(n) =>
        startTime = System.currentTimeMillis()
        makeWorkers(n/10) foreach(_ ! Deal(10))
        totalSimulations = n
      case PlayerCombination(combId) =>
        combinationCounts(combId) += 1
        responsesReceived += 1
        if (responsesReceived == totalSimulations) statisticActor ! GatheredStatistics(combinationCounts.toSeq, totalSimulations)
      case Finished =>
        val stop = System.currentTimeMillis()
        println(s"took ${stop - startTime} ms")
        context.system.shutdown()
    }

    def makeWorkers(n: Int): Seq[ActorRef] = (1 to n) map makeWorker
    def makeWorker(i: Int) = context.actorOf(Props[Dealer], s"worker-$i")
  }

  class Dealer extends Actor {
    def receive = {
      case Deal(n) => for {
        _ <- 1 to n
        hand <- deal(1)
      } sender ! PlayerCombination(handRank(bestCombination(hand)).head)
    }
  }
  
  class Statistic extends Actor {

    val handNames = Seq("High card", "Pair", "2 Pair", "3 Kind", "Straight", "Flush", "Full House", "4 Kind", "Straight Flush", "Royal Flush")

    def receive = {
      case GatheredStatistics(combs, n) =>
        printResults(combs, n)
        sender ! Finished
    }
  }

  def main(args: Array[String]) = {
    val sys = ActorSystem("AkkaClient")
    val manager = sys.actorOf(Props[Manager], "manager")
    manager ! StartSimulation(300000)
  }
}
