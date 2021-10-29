package tilelink
import chisel3._
import chisel3.util.BitPat

// How I wish to have the union type...

trait HasTLTransaction { this: TLChannelParameter => }

trait TLTransactionParameter

trait TLDataTransactionParameter extends TLTransactionParameter {
  val transferSize: Range
  val canCorrupt:   Boolean
}

trait TLAddressTransactionParameter extends TLTransactionParameter {
  // TODO: need BitSet API
  val addressSet: Set[BitPat]
}

trait TLDeniedTransactionParameter extends TLTransactionParameter {
  val canDenied: Boolean
}

trait GetParameter extends TLAddressTransactionParameter

/**
  * Spec 7.2.1
  * Spec 9.5.1
  */
trait CanGet extends HasTLTransaction {
  this: TLOpcodeChannelParameter with TLSourceChannelParameter with TLMaskChannelParameter =>
  val GetParameter: GetParameter
  def Get(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLOpcodeChannel with TLSourceChannel with TLMaskChannel = this match {
    case a: TLChannelAParameter =>
      a.assign(
        opcode = TLOpcode.Get,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = 0.U,
        corrupt = false.B
      )
    case b: TLChannelBParameter =>
      b.assign(
        opcode = TLOpcode.Get,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = 0.U,
        corrupt = false.B
      )
  }
}

trait PutFullDataParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/** Spec 7.2.2
  * Spec 9.5.2
  */
trait CanPutFullData extends HasTLTransaction {
  this: TLOpcodeChannelParameter with TLSourceChannelParameter with TLMaskChannelParameter =>
  val PutFullDataParameter: PutFullDataParameter
  def PutFullData(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLOpcodeChannel with TLSourceChannel with TLMaskChannel = this match {
    case a: TLChannelAParameter =>
      a.assign(
        opcode = TLOpcode.PutFullData,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )
    case b: TLChannelBParameter =>
      b.assign(
        opcode = TLOpcode.PutFullData,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )

  }
}

trait PutPartialDataParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/** Spec 7.2.3
  * Spec 9.5.3
  */
trait CanPutPartialData extends HasTLTransaction {
  this: TLOpcodeChannelParameter with TLSourceChannelParameter with TLMaskChannelParameter =>
  val PutPartialDataParameter: PutPartialDataParameter
  def PutPartialData(
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLOpcodeChannel with TLSourceChannel with TLMaskChannel = this match {
    case a: TLChannelAParameter =>
      a.assign(
        opcode = TLOpcode.PutPartialData,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )
    case b: TLChannelBParameter =>
      b.assign(
        opcode = TLOpcode.PutPartialData,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )

  }
}

trait ArithmeticDataParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/** Spec 8.2.1
  * Spec 9.5.6
  */
trait CanArithmeticData extends HasTLTransaction {
  this: TLOpcodeChannelParameter with TLSourceChannelParameter with TLMaskChannelParameter =>
  val ArithmeticDataParameter: ArithmeticDataParameter
  def ArithmeticData(
    size:    UInt,
    param:   ArithmeticDataParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLOpcodeChannel with TLSourceChannel with TLMaskChannel = this match {
    case a: TLChannelAParameter =>
      a.assign(
        opcode = TLOpcode.ArithmeticData,
        param = param.asUInt(),
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )
    case b: TLChannelBParameter =>
      b.assign(
        opcode = TLOpcode.ArithmeticData,
        param = param.asUInt(),
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )
  }
}

trait LogicalDataParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/** Spec 8.2.2
  * Spec 9.5.7
  */
trait CanLogicalData extends HasTLTransaction {
  this: TLOpcodeChannelParameter with TLSourceChannelParameter with TLMaskChannelParameter =>
  val LogicalDataParameter: LogicalDataParameter
  def LogicalData(
    size:    UInt,
    param:   ArithmeticDataParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLOpcodeChannel with TLSourceChannel with TLMaskChannel = this match {
    case a: TLChannelAParameter =>
      a.assign(
        opcode = TLOpcode.ArithmeticData,
        param = param.asUInt(),
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )
    case b: TLChannelBParameter =>
      b.assign(
        opcode = TLOpcode.LogicalData,
        param = param.asUInt(),
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )
  }
}

trait IntentParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/** Spec 8.2.3
  * Spec 9.5.8
  */
trait CanIntent extends HasTLTransaction {
  this: TLOpcodeChannelParameter with TLSourceChannelParameter with TLMaskChannelParameter =>
  val IntentParameter: IntentParameter
  def Intent(
    size:    UInt,
    param:   IntentParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLOpcodeChannel with TLSourceChannel with TLMaskChannel = this match {
    case a: TLChannelAParameter =>
      a.assign(
        opcode = TLOpcode.Intent,
        param = param.asUInt(),
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )
    case b: TLChannelBParameter =>
      b.assign(
        opcode = TLOpcode.Intent,
        param = param.asUInt(),
        size = size,
        source = source,
        address = address,
        mask = mask,
        data = data,
        corrupt = corrupt
      )

  }
}

trait AcquireBlockParameter extends TLAddressTransactionParameter

/** Spec 9.3.1 */
trait CanAcquireBlock extends HasTLTransaction { this: TLChannelAParameter =>
  val AcquireBlockParameter: AcquireBlockParameter
  def AcquireBlock(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelA =
    assign(
      opcode = TLOpcode.AcquireBlock,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = 0.U,
      corrupt = false.B
    )
}

trait AcquirePermParameter extends TLAddressTransactionParameter

/** Spec 9.3.2 */
trait CanAcquirePerm extends HasTLTransaction { this: TLChannelAParameter =>
  val AcquirePermParameter: AcquirePermParameter
  def AcquirePerm(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelA =
    assign(
      opcode = TLOpcode.AcquirePerm,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = 0.U,
      corrupt = false.B
    )
}

trait ProbeBlockParameter extends TLAddressTransactionParameter

/** Spec 9.3.3 */
trait CanProbeBlock extends HasTLTransaction { this: TLChannelBParameter =>
  val ProbeBlockParameter: ProbeBlockParameter
  def ProbeBlock(
    size:    UInt,
    param:   GrowParam.Type,
    source:  UInt,
    address: UInt,
    mask:    UInt
  ): TLChannelB =
    assign(
      opcode = TLOpcode.ProbeBlock,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      mask = mask,
      data = 0.U,
      corrupt = false.B
    )
}

trait ProbeAckParameter extends TLAddressTransactionParameter

/** Spec 9.3.5 */
trait CanProbeAck extends HasTLTransaction { this: TLChannelCParameter =>
  val ProbeAckParameter: ProbeAckParameter
  def ProbeAck(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt
  ): TLChannelC =
    assign(
      opcode = TLOpcode.ProbeAck,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      data = 0.U,
      corrupt = false.B
    )
}

trait ProbeAckDataParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/** Spec 9.3.6 */
trait CanProbeAckData extends HasTLTransaction { this: TLChannelCParameter =>
  val ProbeAckDataParameter: ProbeAckDataParameter
  def ProbeAckData(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC =
    assign(
      opcode = TLOpcode.ProbeAckData,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      data = data,
      corrupt = corrupt
    )
}

trait GrantParameter extends TLAddressTransactionParameter with TLDeniedTransactionParameter

/** Spec 9.3.7 */
trait CanGrant extends HasTLTransaction { this: TLChannelDParameter =>
  val GrantParameter: GrantParameter
  def Grant(
    size:   UInt,
    param:  CapParam.Type,
    source: UInt,
    sink:   UInt,
    denied: Bool
  ): TLChannelD =
    assign(
      opcode = TLOpcode.Grant,
      param = param.asUInt(),
      size = size,
      source = source,
      sink = sink,
      denied = denied,
      data = 0.U,
      corrupt = false.B
    )
}
trait GrantDataParameter
    extends TLDataTransactionParameter
    with TLAddressTransactionParameter
    with TLDeniedTransactionParameter

/** Spec 9.3.8 */
trait CanGrantData extends HasTLTransaction { this: TLChannelDParameter =>
  val GrantDataParameter: GrantDataParameter
  def GrantData(
    size:    UInt,
    param:   CapParam.Type,
    source:  UInt,
    sink:    UInt,
    denied:  Bool,
    data:    UInt,
    corrupt: Bool
  ): TLChannelD = {
    assign(
      opcode = TLOpcode.GrantData,
      param = param.asUInt(),
      size = size,
      source = source,
      sink = sink,
      denied = denied,
      data = data,
      corrupt = corrupt
    )
  }
}
trait GrantAckParameter extends TLTransactionParameter

/** Spec 9.3.9 */
trait CanGrantAck extends HasTLTransaction { this: TLChannelEParameter =>
  val GrantAckParameter: GrantAckParameter
  def GrantAck(
    sink: UInt
  ): TLChannelE = {
    assign(
      sink = sink
    )
  }
}
trait ReleaseParameter extends TLAddressTransactionParameter

/** Spec 9.3.10 */
trait CanRelease extends HasTLTransaction { this: TLChannelCParameter =>
  val ReleaseParameter: ReleaseParameter
  def Release(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt
  ): TLChannelC = {
    assign(
      opcode = TLOpcode.Release,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      data = 0.U,
      corrupt = false.B
    )
  }
}
trait ReleaseDataParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/** Spec 9.3.11 */
trait CanReleaseData extends HasTLTransaction { this: TLChannelCParameter =>
  val ReleaseDataParameter: ReleaseDataParameter
  def ReleaseData(
    size:    UInt,
    param:   PruneReportParam.Type,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    assign(
      opcode = TLOpcode.ReleaseData,
      param = param.asUInt(),
      size = size,
      source = source,
      address = address,
      data = data,
      corrupt = corrupt
    )
  }
}
trait ReleaseAckParameter
    extends TLDataTransactionParameter
    with TLAddressTransactionParameter
    with TLDeniedTransactionParameter

/** Spec 9.3.12 */
trait CanReleaseAck extends HasTLTransaction { this: TLChannelDParameter =>
  val ReleaseAckParameter: ReleaseAckParameter
  def ReleaseAck(
    size:   UInt,
    source: UInt
  ): TLChannelD = {
    assign(
      opcode = TLOpcode.ReleaseAck,
      param = 0.U,
      size = size,
      source = source,
      sink = 0.U,
      denied = false.B,
      data = 0.U,
      corrupt = false.B
    )
  }
}
trait AccessAckParameter extends TLDataTransactionParameter with TLAddressTransactionParameter {
  val transferSize = Range(0, 0)
}

/**
  * Spec 7.2.4
  * Spec 9.5.4
  */
trait CanAccessAck extends HasTLTransaction { this: TLChannelParameter =>
  val AccessAckParameter: AccessAckParameter
  def AccessAck(
    size:    UInt,
    source:  UInt,
    address: UInt
  ): TLChannelC = this match {
    case c: TLChannelCParameter =>
      c.assign(
        opcode = TLOpcode.AccessAck,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        data = 0.U,
        corrupt = false.B
      )
  }

  def AccessAck(
    size:   UInt,
    source: UInt,
    denied: Bool
  ): TLChannelD = this match {
    case d: TLChannelDParameter =>
      require(AccessAckParameter.isInstanceOf[TLDeniedTransactionParameter])
      d.assign(
        opcode = TLOpcode.AccessAck,
        param = 0.U,
        size = size,
        source = source,
        sink = 0.U,
        denied = denied,
        data = 0.U,
        corrupt = false.B
      )
  }
}
trait AccessAckDataParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/**
  * Spec 7.2.5
  * Spec 9.5.5
  */
trait CanAccessAckData extends HasTLTransaction { this: TLChannelParameter =>
  val AccessAckDataParameter: AccessAckDataParameter
  def AccessAckData(
    size:    UInt,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = this match {
    case c: TLChannelCParameter =>
      c.assign(
        opcode = TLOpcode.AccessAckData,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        data = data,
        corrupt = corrupt
      )
  }
  def AccessAckData(
    size:    UInt,
    source:  UInt,
    denied:  Bool,
    data:    UInt,
    corrupt: Bool
  ): TLChannelD = this match {
    case d: TLChannelDParameter =>
      require(AccessAckDataParameter.isInstanceOf[TLDeniedTransactionParameter])
      d.assign(
        opcode = TLOpcode.AccessAckData,
        param = 0.U,
        size = size,
        source = source,
        sink = 0.U,
        denied = denied,
        data = data,
        corrupt = corrupt
      )
  }
}

trait HintAckParameter extends TLDataTransactionParameter with TLAddressTransactionParameter

/**
  * Spec 8.2.4
  * Spec 9.5.9
  */
trait CanHintAck extends HasTLTransaction { this: TLChannelParameter =>
  val HintAckParameter: HintAckParameter
  def HintAck(
    size:    UInt,
    source:  UInt,
    address: UInt
  ): TLChannelC = this match {
    case c: TLChannelCParameter =>
      c.assign(
        opcode = TLOpcode.HintAck,
        param = 0.U,
        size = size,
        source = source,
        address = address,
        data = 0.U,
        corrupt = false.B
      )
  }

  def HintAck(
    size:   UInt,
    source: UInt,
    denied: Bool
  ): TLChannelD = this match {
    case d: TLChannelDParameter =>
      require(HintAckParameter.isInstanceOf[TLDeniedTransactionParameter])
      d.assign(
        opcode = TLOpcode.HintAck,
        param = 0.U,
        size = size,
        source = source,
        sink = 0.U,
        denied = denied,
        data = 0.U,
        corrupt = false.B
      )
  }
}
