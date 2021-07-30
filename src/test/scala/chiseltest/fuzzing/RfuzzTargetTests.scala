package chiseltest.fuzzing

import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayInputStream

class RfuzzTargetTests extends AnyFlatSpec {
  behavior of "RfuzzTarget"

  val target = "rfuzz"

  it should "execute a single input" in {
    val fuzzer = FIRRTLHandler.firrtlToTarget("src/test/resources/fuzzing/gcd.fir", target, "test_run_dir/rfuzz")
    val input = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).map(_.toByte)
    val (coverage, _) = fuzzer.run(new ByteArrayInputStream(input))
    println(coverage)
    fuzzer.finish()
  }

}
