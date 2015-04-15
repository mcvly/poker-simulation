package org.mcvly.poker

import scala.Ordering.Implicits._

/**
 * @author mcvly
 * @since 11.03.15
 */
object Poker {

  /** find winner(s) in a deal */
  def poker(hands: List[Hand]): List[Hand] = hands.map(bestCombination).groupBy(handRank).maxBy(_._1)._2

  /** Returns sequence where head indicates combination (9-0) and 5 cards by their value in combination */
  def handRank(hand: Hand): Seq[Int] = {
    val (groups, ranks) = group(cardRanks(hand)).unzip
    (groups match {
      case Seq(4,1) => 7 // 4 kind
      case Seq(3, 2) => 6 // full house
      case Seq(3, 1, 1) => 3 // 3 kind
      case Seq(2, 2, 1) => 2 // 2 Pairs
      case Seq(2, 1, 1, 1) => 1 // 1 Pair
      case _ => // Seq(1,1,1,1,1)
        val isStraight = straight(ranks)
        val isFlush = flush(hand)
        if (isStraight && isFlush)
          if (ranks.head == 14) 9 // ROYAL FLUSH
          else 8 // straight flush
        else if (isFlush) 5 // flush
        else if (isStraight) 4 // straight
        else 0 // high card
    }) +: ranks
  }

  /** if hand has more than 5 cards chooses 5 cards that make up best combination */
  def bestCombination(hand: Hand): Hand = {
    assert(hand.cards.size >= 5)
    if (hand.cards.size == 5) hand
    else hand.combinations(5).maxBy(handRank)
  }

  /** Returns a list of [(count, x)...], highest count first, the highest x first */
  def group(ranks: Seq[Int]): Seq[(Int, Int)] = {
    (for (x <- ranks.distinct) yield (ranks.count(_ == x), x)) sortBy(- _._1)
  }

  /** Returns sorted ranks in a hand. Also deals with ace low straight */
  def cardRanks(hand: Hand): Seq[Int] = {
    val ranks = hand.cards.map(_.rank).sortBy(- _)
    // ace may be lower card
    if (ranks == List(14, 5, 4, 3, 2)) List(5, 4, 3, 2, 1) else ranks
  }

  def flush(hand: Hand): Boolean = hand.cards.forall(_.suite == hand.cards(0).suite)
  def straight(ranks: Seq[Int]): Boolean = ranks.size == 5 && ranks.max - ranks.min == 4
}
