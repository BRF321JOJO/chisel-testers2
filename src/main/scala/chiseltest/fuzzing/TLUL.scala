// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.fuzzing

import chiseltest.simulator._
import firrtl._
import firrtl.options.{Dependency, TargetDirAnnotation}
import firrtl.stage._
import logger.{LogLevel, LogLevelAnnotation}
import chiseltest.internal.WriteVcdAnnotation

object TLUL {

  val DefaultAnnotations = Seq(
    RunFirrtlTransformAnnotation(Dependency(pass.MuxToggleCoverage)),
    RunFirrtlTransformAnnotation(Dependency(pass.MetaResetPass)),
    RunFirrtlTransformAnnotation(Dependency(pass.RemovePrintfPass)),
    RunFirrtlTransformAnnotation(Dependency(pass.AssertSignalPass)),
    RunFirrtlTransformAnnotation(Dependency[LowFirrtlEmitter]),
    // if we use verilator, we want to use JNI
    VerilatorUseJNI,
    // debugging output
    // LogLevelAnnotation(LogLevel.Info),
    WriteVcdAnnotation,
  )


  def firrtlToTarget(filename: String, targetDir: String): FuzzTarget = {
    val state = loadFirrtl(filename, targetDir)
    val info = TopmoduleInfo(state.circuit)
    val dut = TreadleSimulator.createContext(state)
    //val dut = VerilatorSimulator.createContext(state)
    new TLULTarget(dut, info)
  }

  private lazy val firrtlStage = new FirrtlStage
  private def loadFirrtl(filename: String, targetDir: String): firrtl.CircuitState = {
    // we need to compile the firrtl file to low firrtl + add mux toggle coverage and meta reset
    val annos = DefaultAnnotations ++ Seq(TargetDirAnnotation(targetDir), FirrtlFileAnnotation(filename))
    val r = firrtlStage.execute(Array(), annos)
    val circuit = r.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    firrtl.CircuitState(circuit, r)
  }
}


//NEW CLASSES + OBJECTS

object Opcode extends Enumeration {
  type Opcode = Value
  val Invalid = Value("0")
  val Wait = Value("1")
  val Read = Value("2")
  val Write = Value("3")
}

object TLULOpcodeAChannel extends Enumeration {
  type TLULOpcodeAChannel = Value
  val PutFull = Value("0")
  val Get = Value("4")
}
object TLULOpcodeDChannel extends Enumeration {
  type TLULOpcodeDChannel = Value
  val AccessAck = Value("0")
  val AccessAckData = Value("1")
}

class Instruction(val opcode: Opcode.Opcode, val address: BigInt = 0, val data: BigInt = 0) {
  def toByteArray(): Array[Byte] = {
    var byteArray = Array(opcode.toString.toByte)

    if (opcode != Opcode.Invalid && opcode != Opcode.Wait) {
      for (i <- 0 to 3) {
        val byte = Array(((address >> i * 8) & 0xFF).toByte)
        byteArray ++= byte
      }

      if (opcode != Opcode.Read) {
        for (i <- 0 to 3) {
          val byte = Array(((data >> i * 8) & 0xFF).toByte)
          byteArray ++= byte
        }
      }

    }
    byteArray
  }



}


//NEW CLASSES + OBJECTS


class TLULTarget(dut: SimulatorContext, info: TopmoduleInfo) extends FuzzTarget {
  val MetaReset = "metaReset"
  require(info.clocks.size == 1, s"Only designs with a single clock are supported!\n${info.clocks}")
  require(info.inputs.exists(_._1 == MetaReset), s"No meta reset in ${info.inputs}")
  require(info.inputs.exists(_._1 == "reset"))

  private val clock = info.clocks.head
  private def step(): Unit = {
    dut.step(clock, 1)
    cycles += 1
  }
  private var cycles: Long = 0
  private var resetCycles: Long = 0
  private var totalTime: Long = 0
  private var coverageTime: Long = 0

  private def setInputsToZero(): Unit = {
    info.inputs.foreach { case (n, _) => dut.poke(n, 0)}
  }

  private def metaReset(): Unit = {
    dut.poke(MetaReset, 1)
    step()
    dut.poke(MetaReset, 0)
    resetCycles += 1
  }

  private def reset(): Unit = {
    dut.poke("reset", 1)
    step()
    dut.poke("reset", 0)
    resetCycles += 1
  }

  private val inputBits = info.inputs.map(_._2).sum
  private val inputSize = scala.math.ceil(inputBits.toDouble / 8.0).toInt

  private def pop(input: java.io.InputStream): Array[Byte] = {
    val r = input.readNBytes(inputSize)
    if(r.size == inputSize) { r } else { Array.emptyByteArray }
  }

  private def getCoverage(): Seq[Byte] = {
    dut.getCoverage().map(_._2).map(v => scala.math.min(v, 255).toByte)
  }

  private val fuzzInputs = info.inputs.filterNot{ case (n, _) => n == MetaReset || n == "reset" }
  private def applyInputs(bytes: Array[Byte]): Unit = {
    var input: BigInt = bytes.zipWithIndex.map { case (b, i) =>  BigInt(b) << (i * 8) }.reduce(_ | _)
    fuzzInputs.foreach { case (name, bits) =>
      val mask = (BigInt(1) << bits) - 1
      val value = input & mask
      input = input >> bits
      dut.poke(name, value)
    }
  }


  //NEW CONSTANTS
  //Number of bits of data read in
  val OT_TL_DW = 32
  //Number of bytes needed for above number of bits
  val OT_TL_DBW = OT_TL_DW >> 3
  //Calculated number x such that 2^x can store above number of bytes
  val OT_TL_SZW: BigInt = math.ceil(math.log(OT_TL_DBW) / math.log(2)).toInt
  //Represents a mask for those bits which are read in (1 means read the bit, 0 means don't read it in)
  val FULL_MASK = (1 << OT_TL_DBW) - 1

  val DEV_RESPONSE_TIMEOUT = 100
  //NEW CONSTANTS


  private def Get(address: BigInt): BigInt = {
    SendTLULRequest(TLULOpcodeAChannel.Get.toString.toInt, address, 0, OT_TL_SZW, FULL_MASK)
    WaitForDeviceResponse()

    val d_data = dut.peek("auto_in_d_bits_data")
    ClearRequest()
    d_data
  }

  //SHARED METHODS
  private def SendTLULRequest(opcode: BigInt, address: BigInt, data: BigInt, size: BigInt, mask: BigInt): Unit = {
    dut.poke("auto_in_a_valid", 1)
    dut.poke("auto_in_a_bits_opcode", opcode)
    dut.poke("auto_in_a_bits_size", size)
    dut.poke("auto_in_a_bits_address", address)
    dut.poke("auto_in_a_bits_data", data)
    dut.poke("auto_in_a_bits_mask", mask)
    dut.poke("auto_in_d_ready", 1)

    WaitForDeviceReady()
  }

  private def ClearRequest(): Unit = {
    ResetH2DSignals()
    dut.poke("auto_in_d_ready", 1)
  }

  private def ResetH2DSignals(): Unit = {
    //Resets a bunch of signals, but not sure what these are
    //Can figure out by seeing all signals after "Resetting all Host-to_device signals" debug message prints
  }

  private def WaitForDeviceReady(): Unit = {
    WaitForDevice("auto_in_a_ready")
  }
  private def WaitForDeviceResponse(): Unit = {
    WaitForDevice("auto_in_d_valid")
  }
  private def WaitForDevice(port: String): Unit = {
    var timeout = DEV_RESPONSE_TIMEOUT
    while (dut.peek(port) == 0) {
      step() //TODO: Do I need only a half step here though?
      if (timeout == 0) {
        throw new Exception("TIMEOUT waiting for device")
      }
      timeout -= 1
    }

  }
  //SHARED METHODS

  //PUT METHODS
  private def PutFull(address: BigInt, data: BigInt): Unit = {
    SendTLULRequest(TLULOpcodeAChannel.PutFull.toString.toInt, address, data, OT_TL_SZW, FULL_MASK)
    RecieveTLULPutReponse()
  }

  private def RecieveTLULPutReponse(): Unit = {
    WaitForDeviceResponse()
    ClearRequest()
  }
  //PUT METHODS


  private def applyInstruction(instruction: Instruction): Unit = {
    instruction.opcode match {
      case Opcode.Wait => {
        //Waits 1 clock cycle
        step()
        println("Wait")
      }
      case Opcode.Read => {
        //Get command
        val readData = Get(instruction.address)
        println("Read: " + readData.toString)
      }
      case Opcode.Write => {
        //PutFull command
        PutFull(instruction.address, instruction.data)
        println("Write: " + instruction.data.toString + " to address " + instruction.address.toString)
      }
      case _ => {
        //Does nothing
        println("Invalid")
      }
    }
  }

  //VARIABLE SIZE VERSION: Only takes the necessary amount of bytes for the instruction
  //Returns next instruction, created by taking the next rightmost bits from input
  private def getInstruction(input: java.io.InputStream): (Instruction, Boolean) = {
    var (opcode, readValid): (Opcode.Opcode, Boolean) = getOpcode(input)

    val ADDRESS_SIZE_BYTES = 4
    val DATA_SIZE_BYTES = 4

    if (readValid) {

      val instruction: Instruction = opcode match {
        case Opcode.Read => {
          val addressBytes: Array[Byte] = input.readNBytes(ADDRESS_SIZE_BYTES)
          if (addressBytes.length != ADDRESS_SIZE_BYTES) {
            readValid = false
          }
          val address: BigInt = addressBytes.zipWithIndex.map { case (b, i) =>  BigInt(b) << (i * 8) }.reduce(_ | _)
          new Instruction(Opcode.Read, address, 0)
        }

        case Opcode.Write => {
          val addressBytes: Array[Byte] = input.readNBytes(ADDRESS_SIZE_BYTES)
          val dataBytes: Array[Byte] = input.readNBytes(DATA_SIZE_BYTES)
          if ((addressBytes.length != ADDRESS_SIZE_BYTES) || (dataBytes.length != DATA_SIZE_BYTES)) {
            readValid = false
          }
          val address: BigInt = addressBytes.zipWithIndex.map { case (b, i) =>  BigInt(b) << (i * 8) }.reduce(_ | _)
          val data: BigInt = dataBytes.zipWithIndex.map { case (b, i) =>  BigInt(b) << (i * 8) }.reduce(_ | _)
          new Instruction(Opcode.Write, address, data)
        }

        case _ => new Instruction(opcode, 0, 0)
      }

      (instruction, readValid)
    } else {
      (new Instruction(Opcode.Invalid, 0, 0), readValid)
    }
  }


  //CONSTANT VERSION: 3 values of byte correspond to valid opcodes, rest are invalid.
  private def getOpcode(input: java.io.InputStream): (Opcode.Opcode, Boolean) = {
    val OPCODE_SIZE_BYTES = 1
    //Reads in next byte from input stream
    val opcodeByte: Array[Byte] = input.readNBytes(OPCODE_SIZE_BYTES)
    if (opcodeByte.length != OPCODE_SIZE_BYTES) {
      return (Opcode.Invalid, false)
    }

    //Matches opcodeByte to corresponding opcodes (1-3). (0, 4-255) match to Invalid.
    val opcode_readValid: (Opcode.Opcode, Boolean) = opcodeByte(0) match {
      case 1 => (Opcode.Wait, true)
      case 2 => (Opcode.Read, true)
      case 3 => (Opcode.Write, true)
      case _ => (Opcode.Invalid, true)
    }
    opcode_readValid
  }

  //NEW METHODS

  override def run(input: java.io.InputStream): Seq[Byte] = {
    val start = System.nanoTime()
    setInputsToZero()
    metaReset()
    reset()
    // we only consider coverage _after_ the reset is done!
    dut.resetCoverage()


    // CHANGES
    var instruction_readValid: (Instruction, Boolean) = getInstruction(input)
    //Loop if last readValid = true
    while (instruction_readValid._2) {
      applyInstruction(instruction_readValid._1)
      instruction_readValid = getInstruction(input)
    }
    //CHANGES


    val startCoverage = System.nanoTime()
    val c = getCoverage()
    val end = System.nanoTime()
    totalTime += (end - start)
    coverageTime += (end - startCoverage)
    c
  }

  private def ms(i: Long): Long = i / 1000 / 1000
  override def finish(verbose: Boolean): Unit = {
    dut.finish()
    if(verbose) {
      println(s"Executed $cycles target cycles (incl. $resetCycles reset cycles).")
      println(s"Total time in simulator: ${ms(totalTime)}ms")
      println(s"Total time for getCoverage: ${ms(coverageTime)}ms (${coverageTime.toDouble / totalTime.toDouble * 100.0}%)")
      val MHz = cycles.toDouble * 1000.0 / totalTime.toDouble
      println(s"$MHz MHz")
    }
  }
}