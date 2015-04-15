package org.mcvly.poker

/**
 * @author mcvly
 * @since 13.03.15
 */

class Hand(val cards: Seq[Card]) {
  def combinations(n: Int) = cards.combinations(n) map Hand.apply
  override def toString = cards.mkString("[", ",", "]")
  def canEqual(other: Any): Boolean = other.isInstanceOf[Hand]
  override def equals(other: Any): Boolean = other match {
    case that: Hand =>
      (that canEqual this) &&
        cards == that.cards
    case _ => false
  }
  override def hashCode(): Int = {
    val state = Seq(cards)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Hand {
  def apply(cards: Seq[Card]) = new Hand(cards)
  def apply(cards: String): Option[Hand] = {
    val parsedCards = cards.split(' ') map parse
    if (parsedCards contains None) None else Some(new Hand(parsedCards map (_.get) sorted))
  }


  def parse(card: String) : Option[Card] = {
    if (card.length != 2) None
    else for {
      suite <- parseSuite(card.tail)
      rank <- rankToInt(card.head)
    } yield Card(rank, suite)
  }

  def rankToInt(s: Char) : Option[Int] = s match {
    case x if '2' to '9' contains x => Some(x - '0')
    case 'T' => Some(10)
    case 'J' => Some(11)
    case 'Q' => Some(12)
    case 'K' => Some(13)
    case 'A' => Some(14)
    case _ => None
  }

  def parseSuite(s: String): Option[Suite] = s match {
    case "H" => Some(Hearts)
    case "D" => Some(Diamonds)
    case "S" => Some(Spades)
    case "C" => Some(Clubs)
    case _ => None
  }
}

case class Card(rank: Int, suite: Suite) {
  def ranks = "--23456789TJQKA"
  override def toString = ranks.substring(rank, rank+1) + (suite match {
    case Hearts => 'H'
    case Diamonds => 'D'
    case Spades => 'S'
    case Clubs => 'C'
  })
}
object Card {
  implicit val cardOrder: Ordering[Card] = new Ordering[Card] {
    override def compare(x: Card, y: Card): Int = x.rank compareTo y.rank
  }
}

sealed trait Suite
case object Hearts extends Suite
case object Diamonds extends Suite
case object Spades extends Suite
case object Clubs extends Suite