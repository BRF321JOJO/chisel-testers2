package chiseltest.fuzzing.pass

import chiseltest.fuzzing.pass
import chiseltest.internal.WriteVcdAnnotation
import chiseltest.simulator.{SimulatorContext, VerilatorSimulator, VerilatorUseJNI, TreadleSimulator}
import firrtl.LowFirrtlEmitter
import firrtl.options.{Dependency, TargetDirAnnotation}
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlSourceAnnotation, FirrtlStage, RunFirrtlTransformAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

class MuxToggleCoveragePassTest extends AnyFlatSpec {
  private val testSrc =
    """circuit test :
      |  module test :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input cond: UInt<1>
      |    output out: UInt<32>
      |
      |    out <= UInt(7)
      |    when cond :
      |      out <= UInt(8)
      |""".stripMargin

  // we run all the passes to ensure they do not negatively interact with each other
  val DefaultAnnotations = Seq(
    RunFirrtlTransformAnnotation(Dependency(pass.MuxToggleCoverage)),
    RunFirrtlTransformAnnotation(Dependency(pass.MetaResetPass)),
    RunFirrtlTransformAnnotation(Dependency(pass.RemovePrintfPass)),
    RunFirrtlTransformAnnotation(Dependency(pass.AssertSignalPass)),
    RunFirrtlTransformAnnotation(Dependency[LowFirrtlEmitter]),
    VerilatorUseJNI,
  )

  private val firrtlStage = new FirrtlStage
  private def load(name: String, src: String, vcd: Boolean = false): SimulatorContext = {
    val annos = DefaultAnnotations ++ Seq(TargetDirAnnotation("test_run_dir/" + name), FirrtlSourceAnnotation(src))
    val r = firrtlStage.execute(Array(), annos)
    val circuit = r.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    val state = firrtl.CircuitState(circuit, r ++ (if(vcd) Some(WriteVcdAnnotation) else None))
    // println(state.circuit.serialize)
    val dut = VerilatorSimulator.createContext(state)
    dut
  }


  it should "correctly calculate mux toggle coverage" in {
    val dut = load("MuxToggleCoverage_should_calculate_coverage", testSrc, vcd = true)

    val signal_values = List((1,1,0), (0,0,0), (0,1,1), (0,0,2), (0,0,2), (0,1,3), (0,1,3), (0,0,4))
    signal_values.foreach{ case (reset, cond, cov) =>
      dut.poke("reset", reset)
      dut.poke("cond", cond)
      dut.step("clock", 1)

      // println(dut.getCoverage())
      assert(dut.getCoverage().head._2 == cov)
    }
    dut.finish()
  }
}
