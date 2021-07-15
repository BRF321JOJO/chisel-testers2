package chiseltest.fuzzing.coverage

import chiseltest.fuzzing._

object CoverageAnalysis extends App{

  def usage = "Usage: java " + this.getClass + " FIRRTL QUEUE OUTPUT_JSON TARGET_KIND"
  require(args.length == 4, usage + "\nNOT: " + args.mkString(" "))

  val firrtlSrc = args(0)
  val queue = os.pwd/os.RelPath(args(1))
  val coverageOutputFile = os.pwd/args(2)

  // Load in chosen DUT to fuzz
  println(s"Loading and instrumenting $firrtlSrc...")

  //Decide what fuzzer to use
  val targetKind = args(3)
  val target: FuzzTarget = targetKind.toLowerCase match {
    case "rfuzz" => Rfuzz.firrtlToTarget(firrtlSrc, "test_run_dir/coverage_rfuzz_with_afl", true)
    case "tlul" => TLUL.firrtlToTarget(firrtlSrc, "test_run_dir/coverage_TLUL_with_afl", true) //*Note: Only use with TLI2C.fir
    case other => throw new NotImplementedError(s"Unknown target $other")
  }

  println("Generating coverage from input queue. Outputting to file " + coverageOutputFile + "...")
  val files = os.list(queue).filter(os.isFile)

  var valid_files = 0

  //Pulls coverage counts from inputted files
  val files_coverageCounts = files.flatMap { inputFile =>
    val in = os.read.inputStream(inputFile)
    val (coverage, valid) = target.run(in)
    in.close()

    if (!valid) {
      valid_files+=1
    }

    if (valid) Some((inputFile, coverage)) else None
  }

  println(s"""Proportion of invalid files is ${valid_files}/${files.length}""")

  outputToFile()
  println("Done!")

  def outputToFile(): Unit = {
    val start_time = os.mtime(files_coverageCounts(0)._1)

    var overallCoverage = Set[Int]()
    val out = new StringBuilder("[")

    val filesCovIter = files_coverageCounts.iterator
    while (filesCovIter.hasNext) {
      val (file, count) = filesCovIter.next()

      overallCoverage = overallCoverage.union(processCoverage(count))
      val coverPoints = count.size/2
      val cumulativeCoverage = overallCoverage.size.toDouble / coverPoints
      if (cumulativeCoverage == 1.0) {
        return
      }

      out.append("{")

      val input_name = file.toString.split("/").last
      out.append(s""""filename": "${input_name}", """)

      //TODO: Fix this hack
      var creation_time = start_time
      if (input_name != "id:000000,orig:in") {
        creation_time = file.toString().split(',').last.toLong / 1000
      }
      val relative_creation_time = (creation_time - start_time)/1000.0
      assert(relative_creation_time >= 0, "Input creation times are not monotonically increasing")
      out.append(s""""creation_time": ${relative_creation_time.toString}, """)
      out.append(s""""cumulative_coverage": ${cumulativeCoverage.toString}""")

      if (filesCovIter.hasNext) {
        out.append("}, \n")
      } else {
        out.append("}]")
      }
    }

    os.write.over(coverageOutputFile, out.substring(0))
  }

  def processCoverage(counts: Seq[Byte]): Set[Int] = {
    var coveredPoints = Set[Int]()
    for (i <- 0 until counts.length/2) {
      if (counts(i*2) >= 1 && counts(i*2+1) >= 1) {
        coveredPoints += i
      }
    }
    coveredPoints
  }

}
