// File: sim/midas/src/main/scala/midas/models/dram/SimplePassThroughModel.scala

package midas.models

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import junctions.NastiKey
import firesim.lib.nasti.NastiParameters

// ============================================================================
// Config Case Class
// ============================================================================
case class SimplePassThroughConfig(
  readLatency: Int = 10,
  writeLatency: Int = 5,
  backendKey: DRAMBackendKey = DRAMBackendKey(4, 4, DRAMMasEnums.backendLatencyBits),
  params: BaseParams
) extends BaseConfig {
  
  def elaborate()(implicit p: Parameters): SimplePassThroughModel = 
    Module(new SimplePassThroughModel(this))
}

// ============================================================================
// MMReg IO (Runtime Configuration Registers)
// ============================================================================
class SimplePassThroughMMRegIO(val cfg: SimplePassThroughConfig) extends MMRegIO(cfg) {
  val readLatency  = Input(UInt(16.W))
  val writeLatency = Input(UInt(16.W))
  
  val registers = Seq(
    (readLatency -> RuntimeSetting(
      cfg.readLatency,
      "Read latency (cycles)",
      min = 1,
      max = Some(1000)
    )),
    (writeLatency -> RuntimeSetting(
      cfg.writeLatency,
      "Write latency (cycles)",
      min = 1,
      max = Some(1000)
    ))
  )
  
  def requestSettings(): Unit = {
    Console.println("Configuring Simple Pass-Through Model")
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
  
  val backend = Module(new DRAMBackend(p(NastiKey), cfg.backendKey))
  
  // =========================================================================
  // READ PATH - Use simplified approach
  // =========================================================================
  
  val readLatencyCounter = RegInit(0.U(16.W))
  val readPending = RegInit(false.B)
  
  // Just capture the AR channel directly
  val pendingAR = Reg(chiselTypeOf(nastiReq.ar.bits))
  
  when(!readPending && nastiReq.ar.valid) {
    readPending := true.B
    pendingAR := nastiReq.ar.bits
    readLatencyCounter := 0.U
  }
  
  nastiReq.ar.ready := !readPending
  
  when(readPending) {
    readLatencyCounter := readLatencyCounter + 1.U
  }
  
  // Issue to backend when latency reached
  backend.io.newRead.valid := readPending && 
                              (readLatencyCounter >= io.mmReg.readLatency) &&
                              backend.io.newRead.ready
  
  // Use the actual AR channel to create metadata
  backend.io.newRead.bits := ReadResponseMetaData(p(NastiKey), pendingAR)
  
  when(backend.io.newRead.fire) {
    readPending := false.B
    readLatencyCounter := 0.U
  }
  
  backend.io.readLatency := 50.U
  
  // =========================================================================
  // WRITE PATH - Use simplified approach
  // =========================================================================
  
  val writeLatencyCounter = RegInit(0.U(16.W))
  val writePending = RegInit(false.B)
  
  // Capture AW and W channels directly
  val pendingAW = Reg(chiselTypeOf(nastiReq.aw.bits))
  val pendingW = Reg(chiselTypeOf(nastiReq.w.bits))
  
  when(!writePending && nastiReq.aw.valid && nastiReq.w.valid) {
    writePending := true.B
    pendingAW := nastiReq.aw.bits
    pendingW := nastiReq.w.bits
    writeLatencyCounter := 0.U
  }
  
  nastiReq.aw.ready := !writePending && nastiReq.w.valid
  nastiReq.w.ready := !writePending && nastiReq.aw.valid
  
  when(writePending) {
    writeLatencyCounter := writeLatencyCounter + 1.U
  }
  
  // Issue to backend when latency reached
  backend.io.newWrite.valid := writePending && 
                               (writeLatencyCounter >= io.mmReg.writeLatency) &&
                               backend.io.newWrite.ready
  
  // Use the actual AW channel to create metadata
  backend.io.newWrite.bits := WriteResponseMetaData(p(NastiKey), pendingAW)
  
  when(backend.io.newWrite.fire) {
    writePending := false.B
    writeLatencyCounter := 0.U
  }
  
  backend.io.writeLatency := 1.U
  
  // =========================================================================
  // CONNECT RESPONSES BACK TO TARGET
  // =========================================================================
  
  rResp <> backend.io.completedRead
  wResp <> backend.io.completedWrite
  
  // =========================================================================
  // PERFORMANCE COUNTERS
  // =========================================================================
  
  val readCounter = RegInit(0.U(64.W))
  val writeCounter = RegInit(0.U(64.W))
  
  when(backend.io.newRead.fire) {
    readCounter := readCounter + 1.U
  }
  
  when(backend.io.newWrite.fire) {
    writeCounter := writeCounter + 1.U
  }
  
  when(tCycle % 10000.U === 0.U) {
    printf("[SimplePassThrough] Reads: %d, Writes: %d\n", readCounter, writeCounter)
  }
  
  backend.io.tCycle := tCycle
}