package tilelink.util

import chisel3._
import tilelink.TLChannel
import tilelink.TLChannelParameter
import chisel3.util.Decoupled
import chisel3.util.Mux1H
import chisel3.util.RegEnable

object Arbiter {
  type Policy = (UInt, Bool) => UInt
  type PolicyFactory = (Integer) => Policy
}

class Arbiter[C <: TLChannel](nInputs: Int, gen: C, p: Arbiter.PolicyFactory) extends Module {
  val inputs = IO(Vec(nInputs, Flipped(Decoupled(gen.cloneType))))
  val output = IO(Decoupled(gen.cloneType))

  val policy = p(nInputs)

  val next = Wire(Bool())
  val acquires = VecInit(inputs.map(_.valid)).asUInt

  val granted = policy(acquires, next)
  val locked = RegEnable(granted, 0.U(nInputs.W), next)
  val cur = Mux(next, granted, locked)

  val beatsLeft = if(gen.channelParameter.maxBeatsPow == 0) {
    None
  } else {
    Some(Reg(UInt(gen.channelParameter.maxBeatsPow.W)))
  }
  
  next := beatsLeft.map(_ === 0.U).getOrElse(true.B)

  val grantedValid = Mux1H(cur, inputs.map(_.valid))
  output.valid := cur.orR && grantedValid
  output.bits := Mux1H(cur, inputs.map(_.bits))

  if(beatsLeft.isDefined) {
    when(output.fire) {
      beatsLeft.get := Mux(next,
        Mux1H(cur, inputs.map(1.U(gen.channelParameter.maxBeatsPow.W) << _.bits.beatsPow)) - 1.U,
        beatsLeft.get - 1.U
      )
    }
  }

  for((input, granted) <- inputs.zip(cur.asBools))
    input.ready := granted && output.ready
}