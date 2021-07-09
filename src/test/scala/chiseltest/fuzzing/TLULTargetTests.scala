package chiseltest.fuzzing

import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayInputStream

class TLULTargetTests extends AnyFlatSpec {
  behavior of "TLULTarget"

  it should "execute a single input" in {
    val fuzzer = TLUL.firrtlToTarget("src/test/resources/fuzzing/TLI2C.fir", "test_run_dir/TLUL")
    //21 bytes required to provide a complete TLI2C input (without HWF Grammar)
    val input = Array(1, 3, 0, 0, 0, 2, 3, 0, 0, 0, 2, 0, 0, 0, 2).map(_.toByte)
    val coverage = fuzzer.run(new ByteArrayInputStream(input))
    println(coverage)
    fuzzer.finish()
  }

  it should "execute a single input, using grammar" in {
    val fuzzer = TLUL.firrtlToTarget("src/test/resources/fuzzing/TLI2C.fir", "test_run_dir/TLUL")
    //21 bytes required to provide a complete TLI2C input (without HWF Grammar)

    val a = new Instruction(Opcode.Wait).toByteArray()
    val b = new Instruction(Opcode.Write, 3, 1).toByteArray()
    val c = new Instruction(Opcode.Read, 3).toByteArray()
    val input = a ++ b ++ c

    val coverage = fuzzer.run(new ByteArrayInputStream(input))
    println(coverage)
    fuzzer.finish()
  }

}
