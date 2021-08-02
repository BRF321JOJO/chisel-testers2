// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.fuzzing

import chiseltest.simulator._
import firrtl._
import firrtl.options.{Dependency, TargetDirAnnotation}
import firrtl.stage._
import logger.{LogLevel, LogLevelAnnotation}

object FIRRTLHandler {
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
  )

  def firrtlToTarget(filename: String, target: String, targetDir: String, annos: AnnotationSeq = Seq.empty): FuzzTarget = {
    val state = loadFirrtl(filename, targetDir, annos)
    val info = TopmoduleInfo(state.circuit)
    val dut = TreadleSimulator.createContext(state)
    //val dut = VerilatorSimulator.createContext(state)

    val fuzzTarget: FuzzTarget = target.toLowerCase() match {
      case "rfuzz" => new RfuzzTarget(dut, info)
      case "tlul" => new TLULTarget(dut, info)
      case other => throw new NotImplementedError(s"Unknown target $other")
    }
    fuzzTarget
  }

  private lazy val firrtlStage = new FirrtlStage
  private def loadFirrtl(filename: String, targetDir: String, annos: AnnotationSeq): firrtl.CircuitState = {
    // we need to compile the firrtl file to low firrtl + add mux toggle coverage and meta reset
    val allAnnos = DefaultAnnotations ++ Seq(TargetDirAnnotation(targetDir), FirrtlFileAnnotation(filename)) ++ annos
    val r = firrtlStage.execute(Array(), allAnnos)
    val circuit = r.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    firrtl.CircuitState(circuit, r)
  }
}
