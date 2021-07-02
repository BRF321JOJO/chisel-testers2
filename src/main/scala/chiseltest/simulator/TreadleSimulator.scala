// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl.annotations.Annotation
import firrtl.stage.FirrtlCircuitAnnotation
import firrtl.{AnnotationSeq, CircuitState}
import treadle.{TreadleTester, TreadleTesterAnnotation}
import treadle.executable.StopException
import treadle.stage.TreadleTesterPhase

object TreadleSimulator extends Simulator {
  override def name: String = "treadle"
  override def isAvailable: Boolean = true
  override def findVersions: Unit = {
    println("treadle is available")
    println(s"version: ${treadle.BuildInfo.version}")
  }

  /** start a new simulation
   *
   * @param state LoFirrtl circuit + annotations
   */
  override def createContext(state: CircuitState): SimulatorContext = {
    val annos = toAnnos(state).map(translateAnnotation)
    val treadleState = (new TreadleTesterPhase).transform(annos)

    val treadleTester = treadleState.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(
        s"TreadleTesterPhase could not build a treadle tester from these annotations" +
          treadleState.mkString("Annotations:\n", "\n  ", "")
      )
    )

    new TreadleContext(treadleTester)
  }

  private def translateAnnotation(a: Annotation): Annotation = a match {
    case chiseltest.internal.WriteVcdAnnotation => treadle.WriteVcdAnnotation
    case other => other
  }

  private def toAnnos(state: CircuitState): AnnotationSeq =
     FirrtlCircuitAnnotation(state.circuit) +: state.annotations
}

private class TreadleContext(tester: TreadleTester) extends SimulatorContext {
  override def sim: Simulator = TreadleSimulator

  require(tester.clockInfoList.size == 1, "Currently only single clock circuits are supported!")
  private def defaultClock = tester.clockInfoList.head.name
  override def step(clock: String, n: Int): Option[SimulatorResults] = {
    require(clock == defaultClock)
    try {
      tester.step(n = n)
      None
    } catch {
      case s : StopException =>
        val exitCode = 1 // TODO!
        Some(SimulatorResults(exitCode))
    }
  }

  override def peek(signal: String): BigInt = tester.peek(signal)

  override def poke(signal: String, value: BigInt): Unit = tester.poke(signal, value)

  override def peekMemory(memory: String, index: Long): BigInt = {
    tester.peekMemory(memory, index.toInt)
  }

  override def pokeMemory(memory: String, index: Long, value: BigInt): Unit = {
    tester.pokeMemory(memory, index.toInt, value)
  }

  override def finish(): SimulatorResults = {
    tester.finish
    SimulatorResults(0)
  }

  override def resetCoverage(): Unit = tester.resetCoverage()

  override def getCoverage():List[(String, Long)] = tester.getCoverage()
}