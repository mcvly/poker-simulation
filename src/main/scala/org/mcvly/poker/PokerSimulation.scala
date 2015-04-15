package org.mcvly.poker

import scala.collection.mutable
import scala.util.Random

/**
 * @author mcvly
 * @since 13.03.15
 */
object PokerSimulation {

  import Poker._

  lazy val deck: Seq[Card] = for {
    r <- 2 to 14
    s <- Seq(Hearts, Diamonds, Spades, Clubs)
  } yield Card(r, s)

  val handNames =
    Seq("High card", "Pair", "2 Pair", "3 Kind", "Straight", "Flush", "Full House", "4 Kind", "Straight Flush", "Royal Flush")


  /** simulate one deal for n hands in texas holdem */
  def deal(numHands: Int): Seq[Hand] = {
    assert(numHands * 2 + 5 < deck.size, "not enough cards in a deck for all hands")
    val shuffledDeck = Random.shuffle(deck)
    val pocketHands = shuffledDeck.sliding(2, 2).take(numHands)
    val openCards = shuffledDeck.drop(52-5)
    pocketHands map (_ ++ openCards) map Hand.apply toSeq
  }

  /** Calculate probability of each combination by simulating n deals */
  def handPercentages(n: Int = 300 * 1000): Unit = {
    val combinationCounts = mutable.ArraySeq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    for {
      _ <- 1 to n
      hand <- deal(1)
    } combinationCounts(handRank(bestCombination(hand)).head) += 1
    printResults(combinationCounts, n)
  }

  def printResults(combinationCounts: Seq[Int], n: Int) =
    for (i <- 9 to 0 by -1) printf("%14s: %6.3f %6d times\n", handNames(i), 100.0 * combinationCounts(i)/n, combinationCounts(i))

  def main(args: Array[String]) = {
    val start = System.currentTimeMillis()
    handPercentages()
    val stop = System.currentTimeMillis()
    println(s"took ${stop - start} ms")
  }
}
