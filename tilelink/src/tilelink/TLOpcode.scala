package tilelink

import chisel3._
import chisel3.experimental.ChiselEnum

object TLOpcode {
  val Get:            UInt = 4.U
  val AccessAckData:  UInt = 1.U
  val PutFullData:    UInt = 0.U
  val PutPartialData: UInt = 1.U
  val AccessAck:      UInt = 0.U
  val ArithmeticData: UInt = 2.U
  val LogicalData:    UInt = 3.U
  val Intent:         UInt = 5.U
  val HintAck:        UInt = 2.U
  val AcquireBlock:   UInt = 6.U
  val AcquirePerm:    UInt = 7.U
  val Grant:          UInt = 4.U
  val GrantData:      UInt = 5.U
  val ProbeBlock:     UInt = 6.U
  val ProbePerm:      UInt = 7.U
  val ProbeAck:       UInt = 4.U
  val ProbeAckData:   UInt = 5.U
  val Release:        UInt = 6.U
  val ReleaseData:    UInt = 7.U
  val ReleaseAck:     UInt = 6.U
}

/** TileLink Spec 1.8.1
  * Table 23
  */
object ArithmeticDataParam extends ChiselEnum {
  val MIN:  ArithmeticDataParam.Type = Value(0.U)
  val MAX:  ArithmeticDataParam.Type = Value(1.U)
  val MINU: ArithmeticDataParam.Type = Value(2.U)
  val MAXU: ArithmeticDataParam.Type = Value(3.U)
  val ADD:  ArithmeticDataParam.Type = Value(4.U)
}

/** TileLink Spec 1.8.1
  * Table 25
  */
object LogicalDataParam extends ChiselEnum {
  val XOR:  LogicalDataParam.Type = Value(0.U)
  val OR:   LogicalDataParam.Type = Value(1.U)
  val AND:  LogicalDataParam.Type = Value(2.U)
  val SWAP: LogicalDataParam.Type = Value(3.U)
}

object IntentParam extends ChiselEnum {
  val PrefetchRead:  IntentParam.Type = Value(0.U)
  val PrefetchWrite: IntentParam.Type = Value(1.U)
}

/** TileLink Spec 1.8.1
  * Table 31 Cap
  */
object CapParam extends ChiselEnum {
  val toT: CapParam.Type = Value(0.U)
  val toB: CapParam.Type = Value(1.U)
  val toN: CapParam.Type = Value(2.U)
}

/** TileLink Spec 1.8.1
  * Table 31 Grow
  */
object GrowParam extends ChiselEnum {
  val NtoB: GrowParam.Type = Value(0.U)
  val NtoT: GrowParam.Type = Value(1.U)
  val BtoT: GrowParam.Type = Value(2.U)
}

/** TileLink Spec 1.8.1
  * Table 31 Prune & Report
  * param can be PruneParam or ReportParam, since it's tricky to implement a union type in Scala 2
  * these should be refactored into one function after Chisel supports Scala 3 in the future.
  */
object PruneReportParam extends ChiselEnum {
  val TtoB: PruneReportParam.Type = Value(0.U)
  val TtoN: PruneReportParam.Type = Value(1.U)
  val BtoN: PruneReportParam.Type = Value(2.U)
  val TtoT: PruneReportParam.Type = Value(3.U)
  val BtoB: PruneReportParam.Type = Value(4.U)
  val NtoN: PruneReportParam.Type = Value(5.U)
}
