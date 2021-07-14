package chiseltest.fuzzing

import chiseltest.fuzzing.coverage.CoverageAnalysis
import org.scalatest.flatspec.AnyFlatSpec

class CoverageAnalysisTests extends AnyFlatSpec{

  it should "process coverage" in {
    val c = Array[Byte](0, 1, 10, 0, 0, 0, 1, 1, 100, 0, 1, 55)
    assert(CoverageAnalysis.processCoverage(c) == Set(3, 5))
  }


}
