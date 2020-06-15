package io.github.scott2000.swapgame

trait UnlockRequirement {
  def id: Option[String]
  def shouldUnlock(score: Int, bestChain: Int): Boolean
  override def toString: String
}

case class ScoreRequirement(val min: Int, _id: Option[String]) extends UnlockRequirement {
  override def id: Option[String] = _id
  override def shouldUnlock(score: Int, bestChain: Int): Boolean = score >= min
  override def toString: String = s"$min points"
}

object GameRequirement extends ScoreRequirement(0, None) {
  override def toString: String = "First game"
}

case class ChainRequirement(val min: Int, _id: Option[String]) extends UnlockRequirement {
  override def id: Option[String] = _id
  override def shouldUnlock(score: Int, bestChain: Int): Boolean = bestChain >= min
  override def toString: String = s"Chain of $min"
}

object TutorialRequirement extends UnlockRequirement {
  override def id: Option[String] = None
  override def shouldUnlock(score: Int, bestChain: Int): Boolean = true
  override def toString: String = "Tutorial completed"
}
