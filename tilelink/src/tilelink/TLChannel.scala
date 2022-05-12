package tilelink

import chisel3._
import Chisel.log2Up

/** Parameter for [[TLChannel]] */
trait TLChannelParameter {
  // Maximum beatsPow
  def maxBeatsPow: Int
}

/** TileLink Spec 1.8.1 Table 14
  *
  * Summary of TileLink routing fields
  *
  * | Channel | Destination | Sequence | Routed By | Provides  | For Use As |
  * | :---:   | :---:       | :---:    | :---:     | :---:     | :---:      |
  * | A       | slave       | request  | a_address | a_source  | d_source   |
  * | B       | master      | request  | b_source  | b_address | c_address  |
  * | C       | slave       | response | c_address | .         |            |
  * | C       | slave       | request  | c_address | c_source  |            |
  * | D       | master      | response | d_source  | .         | d_source   |
  * | D       | master      | request  | d_source  | d_sink    | e_sink     |
  * | E       | slave       | response | e_source  | .         |            |
  */
trait TLChannel extends Bundle {
  val channelParameter: TLChannelParameter

  // Beats in current transfer, repersented in power-of-two
  // The beat count is 2^beatsPow
  def beatsPow: UInt
}

trait TLMasterToSlaveChannel extends TLChannel
trait TLSlaveToMasterChannel extends TLChannel

/** Apply to ABCD channel. */
trait TLOpcodeChannelParameter extends TLChannelParameter

trait TLOpcodeChannel extends TLChannel {
  val channelParameter: TLOpcodeChannelParameter
  val opcode: UInt = UInt(3.W)
  val param:  UInt = UInt(3.W)
}

/** Apply to ABCD channel. */
trait TLDataChannelParameter extends TLChannelParameter {
  val dataWidth: Int
  val sizeWidth: Int

  /** Static option for circuit optimization. */
  val hasCorrupt: Boolean

  /** Size of bytes can be transferred on [[TLDataChannel]] for each beat of message. */
  def beatBytes: Int = dataWidth / 8

  def maxBeatsPow: Int = (BigInt(2).pow(sizeWidth) - 1 - log2Up(beatBytes)).max(0).intValue()
}

trait TLDataChannel extends TLChannel {
  val channelParameter: TLDataChannelParameter
  val data: UInt = UInt(channelParameter.dataWidth.W)
  val size: UInt = UInt(channelParameter.sizeWidth.W)
  val corrupt: Option[Bool] =
    if (channelParameter.hasCorrupt) Some(Bool())
    else None
  
  def beatsPow: UInt = (size - log2Up(channelParameter.beatBytes).U).max(0.U)
}

/** Apply to AB channel. */

trait TLMaskChannelParameter extends TLAddressChannelParameter {
  /** width for [[TLMaskChannel.mask]]. */
  def maskWidth: Int = dataWidth / 8
}

trait TLMaskChannel extends TLChannel with TLAddressChannel with TLDataChannel {
  override val channelParameter: TLMaskChannelParameter
  val mask: UInt = UInt(channelParameter.maskWidth.W)
}

/** only D channel can deny. */

trait TLDeniedChannelParameter extends TLChannelParameter {
  /** Static option for circuit optimization. */
  val hasDenied: Boolean
}

trait TLDeniedChannel extends TLChannel {
  override val channelParameter: TLDeniedChannelParameter
  val denied: Option[Bool] =
    if (channelParameter.hasDenied) Some(Bool())
    else None
}

/** Apply to ABC channel. */

trait TLAddressChannelParameter extends TLDataChannelParameter {

  /** width for [[TLAddressChannel.address]]. */
  val addressWidth: Int
}

trait TLAddressChannel extends TLChannel with TLDataChannel {
  override val channelParameter: TLAddressChannelParameter
  val address: UInt = UInt(channelParameter.addressWidth.W)
}

/** Apply to ABCD channel. */

trait TLSourceChannelParameter extends TLDataChannelParameter {

  /** width for [[TLSourceChannel.source]]. */
  val sourceWidth: Int
}

trait TLSourceChannel extends TLChannel with TLDataChannel {
  override val channelParameter: TLSourceChannelParameter
  val source: UInt = UInt(channelParameter.sourceWidth.W)
}

/** Apply to DE channel. */

trait TLSinkChannelParameter extends TLChannelParameter {

  /** width for [[TLSinkChannel.sink]]. */
  val sinkWidth: Int
}

trait TLSinkChannel extends TLChannel {
  override val channelParameter: TLSinkChannelParameter
  val sink: UInt = UInt(channelParameter.sinkWidth.W)
}

/** TileLink A Channel, refer to Spec 3.3 */
trait TLChannelAParameter
    extends TLChannelParameter
    with TLOpcodeChannelParameter
    with TLSourceChannelParameter
    with TLAddressChannelParameter
    with TLMaskChannelParameter
    with TLDataChannelParameter { p =>

  /** Notice: for custom transaction and parameter,
    * this should be override to provide a custom transaction,
    * see tests.
    */
  def assign(
    opcode:  UInt,
    param:   UInt,
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelA = {
    val a: TLChannelA = Wire(new TLChannelA {
      override val channelParameter: TLChannelAParameter = p
    })
    a.opcode := opcode
    a.param := param
    a.size := size
    a.source := source
    a.address := address
    a.mask := mask
    a.data := data
    a.corrupt.foreach(_ := corrupt)
    a
  }
}

trait TLChannelA
    extends TLChannel
    with TLOpcodeChannel
    // Request provides `a.source` for use as `d.source`
    with TLSourceChannel
    // Request routed by `a.address`
    with TLAddressChannel
    with TLMaskChannel
    with TLDataChannel
    with TLMasterToSlaveChannel {
  override val channelParameter: TLChannelAParameter
}

/** TileLink B Channel, refer to Spec 3.4 */
trait TLChannelBParameter
    extends TLChannelParameter
    with TLOpcodeChannelParameter
    with TLSourceChannelParameter
    with TLAddressChannelParameter
    with TLMaskChannelParameter
    with TLDataChannelParameter { p =>

  /** Notice: for custom transaction and parameter,
    * this should be override to provide a custom transaction,
    * see tests.
    */
  def assign(
    opcode:  UInt,
    param:   UInt,
    size:    UInt,
    source:  UInt,
    address: UInt,
    mask:    UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelB = {
    val b: TLChannelB = Wire(new TLChannelB {
      override val channelParameter: TLChannelBParameter = p
    })
    b.opcode := opcode
    b.param := param
    b.size := size
    b.source := source
    b.address := address
    b.mask := mask
    b.data := data
    b.corrupt.foreach(_ := corrupt)
    b
  }
}

trait TLChannelB
    extends TLChannel
    with TLOpcodeChannel
    // Request routed by `b.source`
    with TLSourceChannel
    // Request provides `b.address` for use as `c.address`
    with TLAddressChannel
    with TLMaskChannel
    with TLDataChannel
    with TLSlaveToMasterChannel {
  override val channelParameter: TLChannelBParameter
}

/** TileLink C Channel, refer to Spec 3.5 */
trait TLChannelCParameter
    extends TLChannelParameter
    with TLOpcodeChannelParameter
    with TLSourceChannelParameter
    with TLAddressChannelParameter
    with TLDataChannelParameter { p =>

  /** Notice: for custom transaction and parameter,
    * this should be override to provide a custom transaction,
    * see tests.
    */
  def assign(
    opcode:  UInt,
    param:   UInt,
    size:    UInt,
    source:  UInt,
    address: UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelC = {
    val c: TLChannelC = Wire(new TLChannelC {
      override val channelParameter: TLChannelCParameter = p
    })
    c.opcode := opcode
    c.param := param
    c.size := size
    c.source := source
    c.address := address
    c.data := data
    c.corrupt.foreach(_ := corrupt)
    c
  }
}

trait TLChannelC
    extends TLChannel
    with TLOpcodeChannel
    // Request provides `c.source` for use as `d.source`
    with TLSourceChannel
    // Response routed by `c.address` provided by `a.address`
    // Request routed by `c.address`
    with TLAddressChannel
    with TLDataChannel
    with TLMasterToSlaveChannel {
  override val channelParameter: TLChannelCParameter
}

/** TileLink D Channel, refer to Spec 3.6 */
trait TLChannelDParameter
    extends TLChannelParameter
    with TLOpcodeChannelParameter
    with TLSourceChannelParameter
    with TLSinkChannelParameter
    with TLDataChannelParameter
    with TLDeniedChannelParameter { p =>

  /** Notice: for custom transaction and parameter,
    * this should be override to provide a custom transaction,
    * see tests.
    */
  def assign(
    opcode:  UInt,
    param:   UInt,
    size:    UInt,
    source:  UInt,
    sink:    UInt,
    denied:  UInt,
    data:    UInt,
    corrupt: Bool
  ): TLChannelD = {
    val d: TLChannelD = Wire(new TLChannelD {
      override val channelParameter: TLChannelDParameter = p
    })
    d.opcode := opcode
    d.param := param
    d.size := size
    d.source := source
    d.sink := sink
    d.denied.foreach(_ := denied)
    d.data := data
    d.corrupt.foreach(_ := corrupt)
    d
  }
}

trait TLChannelD
    extends TLChannel
    with TLOpcodeChannel
    // Response routed by `d.source` provided by `a.source`
    // Request routed by `d.source`
    with TLSourceChannel
    // Request provides `d.sink` for use as `e.sink`
    with TLSinkChannel
    with TLDataChannel
    with TLDeniedChannel
    with TLSlaveToMasterChannel {
  override val channelParameter: TLChannelDParameter
}

/** TileLink E Channel, refer to Spec 3.7 */
trait TLChannelEParameter extends TLChannelParameter with TLSinkChannelParameter { p =>
  def maxBeatsPow: Int = 0

  /** Notice: for custom transaction and parameter,
    * this should be override to provide a custom transaction,
    * see tests.
    */
  def assign(
    sink: UInt
  ): TLChannelE = {
    val e: TLChannelE = Wire(new TLChannelE {
      override val channelParameter: TLChannelEParameter = p
    })
    e.sink := sink
    e
  }
}

trait TLChannelE
    extends TLChannel
    // Response routed by `e.sink`
    with TLSinkChannel
    with TLMasterToSlaveChannel {
  override val channelParameter: TLChannelEParameter

  // TL Channel E always have a single beat
  override def beatsPow: UInt = 0.U
}
