package io.github.scott2000.swapgame

trait UnlockRequirement {
  def id: Option[String]
  def shouldUnlock(score: Int, bestChain: Int, bestCombo: Int): Boolean
  override def toString: String
}

object NoRequirement extends UnlockRequirement {
  override def id: Option[String] = None
  override def shouldUnlock(score: Int, bestChain: Int, bestCombo: Int): Boolean = true
  override def toString: String = "Default unlock"
}

case class ScoreRequirement(min: Int, _id: Option[String]) extends UnlockRequirement {
  override def id: Option[String] = _id
  override def shouldUnlock(score: Int, bestChain: Int, bestCombo: Int): Boolean = score >= min
  override def toString: String = s"$min points"
}

case class ChainRequirement(min: Int, _id: Option[String]) extends UnlockRequirement {
  override def id: Option[String] = _id
  override def shouldUnlock(score: Int, bestChain: Int, bestCombo: Int): Boolean = bestChain >= min
  override def toString: String = s"Chain of $min"
}

case class ComboRequirement(min: Int, _id: Option[String]) extends UnlockRequirement {
  override def id: Option[String] = _id
  override def shouldUnlock(score: Int, bestChain: Int, bestCombo: Int): Boolean = bestCombo >= min
  override def toString: String = s"Combo of $min"
}