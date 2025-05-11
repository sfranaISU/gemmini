package gemmini

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters


class AESWrapper[T <: Data](dataWidth: Int, metaType: T)(implicit p: Parameters) extends Module {

  private def freshMeta: T = metaType.cloneType

  class AESWrapperInput extends Bundle {
    val data = UInt(dataWidth.W)
    val meta = freshMeta //.cloneType //chiselTypeOf(metaType)
  }
  class AESWrapperOutput extends Bundle {
    val data = UInt(dataWidth.W)
    val meta = freshMeta //.cloneType
  }

  val io = IO(new Bundle {
    val key     = Input(UInt(256.W))
    val encrypt = Input(Bool())
    val in  = Flipped(Decoupled(new AESWrapperInput))
    val out =        Decoupled(new AESWrapperOutput)
  })

  val aes = Module(new AesCipherCoreDriver)

  // data & metadata FIFOs
  val data_q = Module(new Queue(UInt(dataWidth.W),  entries = 4, pipe = true))
  val meta_q = Module(new Queue(freshMeta,         entries = 4, pipe = true))

  // enqueue
  data_q.io.enq.valid := io.in.valid
  data_q.io.enq.bits  := io.in.bits.data
  meta_q.io.enq.valid := io.in.valid
  meta_q.io.enq.bits  := io.in.bits.meta
  io.in.ready         := data_q.io.enq.ready && meta_q.io.enq.ready

  // drive AES in
  aes.io.in.valid   := data_q.io.deq.valid && meta_q.io.deq.valid
  aes.io.in.bits.encrypt := io.encrypt
  aes.io.in.bits.data    := data_q.io.deq.bits
  aes.io.in.bits.key     := io.key
  data_q.io.deq.ready := aes.io.in.fire
  meta_q.io.deq.ready := aes.io.in.fire

  // hold metadata through encryption
  val meta_out_q = Module(new Queue(freshMeta, entries = 4, pipe = true))
  meta_out_q.io.enq.valid := aes.io.in.fire
  meta_out_q.io.enq.bits  := meta_q.io.deq.bits
  meta_out_q.io.deq.ready := io.out.ready

  // drive output
  io.out.valid         := aes.io.out.valid && meta_out_q.io.deq.valid
  io.out.bits.data     := aes.io.out.bits.data
  io.out.bits.meta     := meta_out_q.io.deq.bits
  aes.io.out.ready     := io.out.ready && meta_out_q.io.deq.valid


  when (aes.io.in.fire) {
    printf(":AES_WRAPPER: Encrypt(%d) Data(0x%x)\n", aes.io.in.bits.encrypt, aes.io.in.bits.data)
  }
}
