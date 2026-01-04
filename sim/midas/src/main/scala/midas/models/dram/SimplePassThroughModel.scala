// File: sim/midas/src/main/scala/midas/models/dram/SimplePassThroughModel.scala

package midas.models

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import junctions.NastiKey
import midas.widgets._

// ============================================================================
// Config Case Class
// ============================================================================
case class SimplePassThroughConfig(
  readLatency: Int = 10,   // Cycles to wait before issuing read to backend
  writeLatency: Int = 5,   // Cycles to wait before issuing write to backend
  params: BaseParams       // Standard FASED functional model params
) extends BaseConfig {
  
  def elaborate()(implicit p: Parameters): SimplePassThroughModel = 
    Module(new SimplePassThroughModel(this))
}

// ============================================================================
// MMReg IO (Runtime Configuration Registers)
// ============================================================================
class SimplePassThroughMMRegIO(val cfg: SimplePassThroughConfig) extends BaseMMRegIO(cfg) {
  val readLatency  = Input(UInt(16.W))
  val writeLatency = Input(UInt(16.W))
  
  val registers = baseParams ++ Seq(
    (readLatency -> RuntimeSetting(
      default = cfg.readLatency,
      query   = "Read latency (cycles)",
      min     = 1,
      max     = Some(1000)
    )),
    (writeLatency -> RuntimeSetting(
      default = cfg.writeLatency,
      query   = "Write latency (cycles)",
      min     = 1,
      max     = Some(1000)
    ))
  )
  
  def requestSettings(): Unit = {
    Console.println("Configuring Simple Pass-Through Model")
    setBaseSettings()
  }
}

// ============================================================================
// IO Bundle
// ============================================================================
class SimplePassThroughIO(val cfg: SimplePassThroughConfig)(implicit p: Parameters) 
    extends TimingModelIO()(p) {
  val mmReg = new SimplePassThroughMMRegIO(cfg)
}

// ============================================================================
// The Actual Timing Model
// ============================================================================
class SimplePassThroughModel(cfg: SimplePassThroughConfig)(implicit p: Parameters)
    extends TimingModel(cfg)(p) {
  
  val longName = "Simple Pass-Through Model"
  
  def printTimingModelGenerationConfig: Unit = {
    println(s"  Read Latency: ${cfg.readLatency} cycles")
    println(s"  Write Latency: ${cfg.writeLatency} cycles")
  }
  
  lazy val io = IO(new SimplePassThroughIO(cfg))
  
  // Backend handles actual host DRAM access
  val backend = Module(new DRAMBackend(
    p(NastiKey), 
    DRAMBackendKey(
      maxReads = cfg.params.maxReads,
      maxWrites = cfg.params.maxWrites,
      beatCounters = cfg.params.beatCounters
    )
  ))
  
  // =========================================================================
  // READ PATH
  // =========================================================================
  
  // Queue to buffer incoming read requests from target
  val readQueue = Module(new Queue(new ReadRequest(p(NastiKey)), entries = 16))
  
  // Counter for read latency
  val readLatencyCounter = RegInit(0.U(16.W))
  val readPending = RegInit(false.B)
  val pendingReadReq = Reg(new ReadRequest(p(NastiKey)))
  
  // Accept reads from target (nastiReq comes from TimingModel base class)
  readQueue.io.enq.valid := nastiReq.ar.valid
  readQueue.io.enq.bits.addr := nastiReq.ar.bits.addr
  readQueue.io.enq.bits.id := nastiReq.ar.bits.id
  readQueue.io.enq.bits.size := nastiReq.ar.bits.size
  readQueue.io.enq.bits.len := nastiReq.ar.bits.len
  nastiReq.ar.ready := readQueue.io.enq.ready
  
  // Dequeue and start latency counter
  when(!readPending && readQueue.io.deq.valid) {
    readPending := true.B
    pendingReadReq := readQueue.io.deq.bits
    readLatencyCounter := 0.U
    readQueue.io.deq.ready := true.B
  }.otherwise {
    readQueue.io.deq.ready := false.B
  }
  
  // Count up latency
  when(readPending) {
    readLatencyCounter := readLatencyCounter + 1.U
  }
  
  // Issue to backend when latency reached
  backend.io.newRead.valid := readPending && 
                              (readLatencyCounter >= io.mmReg.readLatency) &&
                              backend.io.newRead.ready
  backend.io.newRead.bits := ReadResponseMetaData(p(NastiKey), pendingReadReq)
  
  when(backend.io.newRead.fire) {
    readPending := false.B
    readLatencyCounter := 0.U
  }
  
  // Backend latency (time for host DRAM to respond)
  backend.io.readLatency := 50.U  // Fixed 50 cycle host DRAM latency
  
  // =========================================================================
  // WRITE PATH
  // =========================================================================
  
  // Queue for write addresses
  val writeAddrQueue = Module(new Queue(new WriteRequest(p(NastiKey)), entries = 16))
  
  // Queue for write data (must match address queue)
  val writeDataQueue = Module(new Queue(new WriteData(p(NastiKey)), entries = 16))
  
  // Counter for write latency
  val writeLatencyCounter = RegInit(0.U(16.W))
  val writePending = RegInit(false.B)
  val pendingWriteReq = Reg(new WriteRequest(p(NastiKey)))
  val pendingWriteData = Reg(new WriteData(p(NastiKey)))
  
  // Accept write addresses from target
  writeAddrQueue.io.enq.valid := nastiReq.aw.valid
  writeAddrQueue.io.enq.bits.addr := nastiReq.aw.bits.addr
  writeAddrQueue.io.enq.bits.id := nastiReq.aw.bits.id
  writeAddrQueue.io.enq.bits.size := nastiReq.aw.bits.size
  writeAddrQueue.io.enq.bits.len := nastiReq.aw.bits.len
  nastiReq.aw.ready := writeAddrQueue.io.enq.ready
  
  // Accept write data from target
  writeDataQueue.io.enq.valid := nastiReq.w.valid
  writeDataQueue.io.enq.bits.data := nastiReq.w.bits.data
  writeDataQueue.io.enq.bits.strb := nastiReq.w.bits.strb
  writeDataQueue.io.enq.bits.last := nastiReq.w.bits.last
  nastiReq.w.ready := writeDataQueue.io.enq.ready
  
  // Dequeue when both address and data available
  when(!writePending && writeAddrQueue.io.deq.valid && writeDataQueue.io.deq.valid) {
    writePending := true.B
    pendingWriteReq := writeAddrQueue.io.deq.bits
    pendingWriteData := writeDataQueue.io.deq.bits
    writeLatencyCounter := 0.U
    writeAddrQueue.io.deq.ready := true.B
    writeDataQueue.io.deq.ready := true.B
  }.otherwise {
    writeAddrQueue.io.deq.ready := false.B
    writeDataQueue.io.deq.ready := false.B
  }
  
  // Count up latency
  when(writePending) {
    writeLatencyCounter := writeLatencyCounter + 1.U
  }
  
  // Issue to backend when latency reached
  backend.io.newWrite.valid := writePending && 
                               (writeLatencyCounter >= io.mmReg.writeLatency) &&
                               backend.io.newWrite.ready
  backend.io.newWrite.bits := WriteResponseMetaData(p(NastiKey), pendingWriteReq, pendingWriteData)
  
  when(backend.io.newWrite.fire) {
    writePending := false.B
    writeLatencyCounter := 0.U
  }
  
  // Backend write latency (host DRAM doesn't really have write latency in FASED)
  backend.io.writeLatency := 1.U
  
  // =========================================================================
  // CONNECT RESPONSES BACK TO TARGET
  // =========================================================================
  
  // Backend provides completed read/write responses
  // These go back to the target (rResp/wResp from TimingModel base class)
  rResp <> backend.io.completedRead
  wResp <> backend.io.completedWrite
  
  // =========================================================================
  // PERFORMANCE COUNTERS (Optional but useful)
  // =========================================================================
  
  val readCounter = RegInit(0.U(64.W))
  val writeCounter = RegInit(0.U(64.W))
  
  when(backend.io.newRead.fire) {
    readCounter := readCounter + 1.U
  }
  
  when(backend.io.newWrite.fire) {
    writeCounter := writeCounter + 1.U
  }
  
  // These can be exposed via MMIOs for runtime monitoring
  when(tCycle % 10000.U === 0.U) {
    printf("[SimplePassThrough] Reads: %d, Writes: %d\n", readCounter, writeCounter)
  }
  
  // Connect backend to host time
  backend.io.tCycle := tCycle
}

// ============================================================================
// Helper Classes (if not already defined in FASED)
// ============================================================================

class ReadRequest(nastiParams: junctions.NastiParameters) extends Bundle {
  val addr = UInt(nastiParams.addrBits.W)
  val id = UInt(nastiParams.idBits.W)
  val size = UInt(3.W)
  val len = UInt(8.W)
}

class WriteRequest(nastiParams: junctions.NastiParameters) extends Bundle {
  val addr = UInt(nastiParams.addrBits.W)
  val id = UInt(nastiParams.idBits.W)
  val size = UInt(3.W)
  val len = UInt(8.W)
}

class WriteData(nastiParams: junctions.NastiParameters) extends Bundle {
  val data = UInt(nastiParams.dataBits.W)
  val strb = UInt((nastiParams.dataBits/8).W)
  val last = Bool()
}