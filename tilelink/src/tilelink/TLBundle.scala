package tilelink

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}

import scala.collection.immutable.ListMap

case class TLBundleParameter(
  a: TLChannelAParameter,
  b: Option[TLChannelBParameter],
  c: Option[TLChannelCParameter],
  d: TLChannelDParameter,
  e: Option[TLChannelEParameter]) {
  require(
    (b.isDefined && c.isDefined && e.isDefined) || (b.isEmpty && c.isEmpty && e.isEmpty),
    "only AD/ABCDE channels is allowed."
  )

  val isTLC: Boolean = b.isDefined && c.isDefined && e.isDefined
}

class TLDecoupledBundle(val bundleParameter: TLBundleParameter) extends Record {
  // TL-UL and TL-UH
  lazy val a: DecoupledIO[TLChannelA] = Decoupled(new TLChannelA(bundleParameter.a))
  lazy val d: DecoupledIO[TLChannelD] = Flipped(Decoupled(new TLChannelD(bundleParameter.d)))

  // TL-C, lazy val will be instantiate at evaluating [[elements]] or user call them by mistake.
  lazy val b: DecoupledIO[TLChannelB] =
    if (bundleParameter.isTLC) Flipped(Decoupled(new TLChannelB(bundleParameter.b.get)))
    else throw TLCNotInThisBundle(bundleParameter)
  lazy val c: DecoupledIO[TLChannelC] =
    if (bundleParameter.isTLC) Flipped(Decoupled(new TLChannelC(bundleParameter.c.get)))
    else throw TLCNotInThisBundle(bundleParameter)
  lazy val e: DecoupledIO[TLChannelE] =
    if (bundleParameter.isTLC) Flipped(Decoupled(new TLChannelE(bundleParameter.e.get)))
    else throw TLCNotInThisBundle(bundleParameter)

  override val elements: ListMap[String, DecoupledIO[TLChannel]] =
    if (bundleParameter.isTLC) ListMap("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e)
    else ListMap("a" -> a, "d" -> d)

  override def cloneType: this.type = new TLDecoupledBundle(bundleParameter).asInstanceOf[this.type]
}

object TLBundle {

  /** Generate a Decoupled TileLink Bundle based on [[TLBundleParameter]].
    * Handshake is defined in spec.
    */
  def decoupled(bundleParameter: TLBundleParameter) = new TLDecoupledBundle(bundleParameter)
}
