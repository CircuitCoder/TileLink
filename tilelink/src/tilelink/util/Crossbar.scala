import chisel3._
import chisel3.util.experimental._
import tilelink._
import chisel3.util.BitPat

class Crossbar(
  nInputs: Int,
  nOutputs: Int,
  inputRanges: Iterable[BitSet],
  outputRanges: Iterable[BitSet],
  param: TLBundleParameter
) extends Module {
  val inputs = for(i <- 0 until nInputs) yield IO(TLBundle.decoupled(param))
  val outputs = for(i <- 0 until nOutputs) yield IO(Flipped(TLBundle.decoupled(param)))

  val entries = outputRanges.toSeq

  // Check all tbl entries are disjoint
  for(i <- 0 until entries.length)
    for(j <- 0 until entries.length)
      if(i != j)
        require(!entries(i).overlap(entries(j)), s"Address for output $i and $j are intersecting")
}

case class CrossbarBuilder(
  val inputs: Seq[(TLDecoupledBundle, Option[BitSet])],
  val outputs: Seq[(BitSet, TLDecoupledBundle)],
) {
  def <-<(input: TLDecoupledBundle) = CrossbarBuilder(inputs :+ input -> None, outputs)
  def <-<(pair: (TLDecoupledBundle, BitSet)) = CrossbarBuilder(inputs :+ pair._1 -> Some(pair._2), outputs)
  def >->(output: (BitSet, TLDecoupledBundle)) = CrossbarBuilder(inputs, outputs :+ output)
  def build = Crossbar.on(inputs, outputs)
}

object Crossbar {
  def on(
    inputs: Seq[(TLDecoupledBundle, Option[BitSet])],
    outputs: Seq[(BitSet, TLDecoupledBundle)]
  ): Crossbar = {
    // TODO: check all parameters are compatible
    // TODO: check nInputs / nOutputs > 0

    val param = inputs(0)._1.bundleParameter

    val inputRanges = inputs.map(_._2.getOrElse(BitPat.dontCare(param.a.addressWidth)))
    val outputRanges = outputs.map(_._1)

    val cb = Module(new Crossbar(inputs.length, outputs.size, inputRanges, outputRanges, param))
    for((iSrc, iSink) <- inputs.map(_._1).zip(cb.inputs)) iSink <> iSrc
    for((oSink, oSrc) <- outputs.map(_._2).zip(cb.outputs)) oSink <> oSrc

    cb
  }

  def builder = CrossbarBuilder(Seq(), Seq())
}

object Test {
  def test(param: TLBundleParameter) {
    val i1 = TLBundle.decoupled(param)
    val i2 = TLBundle.decoupled(param)
    val o1 = Flipped(TLBundle.decoupled(param))
    val o2 = Flipped(TLBundle.decoupled(param))

    val cb = (
      Crossbar.builder
        <-< i1 -> BitPat("b1????????????????????????????????")
        <-< i2
        >-> BitPat("b0????????????????????????????????") -> o1
        >-> BitPat("b1????????????????????????????????") -> o2
      ).build
  }
}