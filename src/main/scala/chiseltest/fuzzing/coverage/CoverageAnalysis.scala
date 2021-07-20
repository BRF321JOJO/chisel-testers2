package chiseltest.fuzzing.coverage

import chiseltest.fuzzing._

object CoverageAnalysis extends App{

  def usage = "Usage: java " + this.getClass + " FIRRTL AFL_OUT_FOLDER OUTPUT_JSON TARGET_KIND"
  require(args.length == 4, usage + "\nNOT: " + args.mkString(" "))

  //Parse arguments to scripts
  val firrtlSrc = args(0)
  val queue = os.pwd/os.RelPath(args(1) + "/queue")
  val end_time_file = os.pwd/os.RelPath(args(1) + "/end_time")
  val outputJSON = os.pwd/args(2)


  //Select and instrument chosen fuzzer
  println(s"Loading and instrumenting $firrtlSrc...")

  val targetKind = args(3)
  val target: FuzzTarget = targetKind.toLowerCase match {
    case "rfuzz" => Rfuzz.firrtlToTarget(firrtlSrc, "test_run_dir/coverage_rfuzz_with_afl", true)
    case "tlul" => TLUL.firrtlToTarget(firrtlSrc, "test_run_dir/coverage_TLUL_with_afl", true) //*Note: Only use with TLI2C.fir
    case other => throw new NotImplementedError(s"Unknown target $other")
  }


  println("Generating coverage from input queue. Outputting to file " + outputJSON + "...")

  //Read in inputs files from queue and generate list of input-coverage pairs (ignores invalid coverage)
  val queue_files = os.list(queue).filter(os.isFile)
  var invalid_files: Int = 0

  val files_coverageCounts = queue_files.flatMap { inputFile =>
    val in = os.read.inputStream(inputFile)
    val (coverage, valid) = target.run(in)
    in.close()

    if (valid) {
      Some((inputFile, coverage))
    } else {
      invalid_files+=1
      None
    }
  }

  //Prints proportion of invalid files
  assert(invalid_files/queue_files.length != 1, s"""No inputs in ${queue} are valid!""")
  println(s"""Proportion of invalid files is ${invalid_files}/${queue_files.length}""")


  //Builds JSON file from coverage data
  val out = new StringBuilder("{")
  appendCoverageData(out)
  out.append(", \n")
  appendEndTime(out)
  out.append("}")
  os.write.over(outputJSON, out.substring(0))


  println("Done!")


  //Append end time to JSON file
  def appendEndTime(out: StringBuilder): Unit = {
    val source = scala.io.Source.fromFile(end_time_file.toString())
    val data = try source.mkString.toLong finally source.close()

    assert(start_time != 0L, "Start time is not initialized")
    out.append(s""""end_time": ${(data - start_time)/1000.0}""")
  }

  private var start_time = 0L

  //Append coverage data to JSON file
  def appendCoverageData(out: StringBuilder): Unit = {
    var overallCoverage = Set[Int]()
    var previous_time = 0L

    out.append(s""""coverage_data": \n[""")

    val filesCovIter = files_coverageCounts.iterator
    while (filesCovIter.hasNext) {
      val (file, count) = filesCovIter.next()

      out.append("\t{")

      //Add filename to JSON file
      val input_name = file.toString.split("/").last
      out.append(s""""filename": "${input_name}", """)

      //Add relative creation time (seconds) to JSON file
      val creation_time = file.toString().split(',').last.toLong
      if (input_name.split(',')(0) == "id:000000") {
        start_time = creation_time
      }
      assert(creation_time >= previous_time, "Input creation times are not monotonically increasing")
      previous_time = creation_time

      val relative_creation_time = (creation_time - start_time)/1000.0
      out.append(s""""creation_time": ${relative_creation_time.toString}, """)


      //Add cumulative coverage to JSON file
      overallCoverage = overallCoverage.union(processCoverage(count))
      val coverPoints = count.size/2
      val cumulativeCoverage = overallCoverage.size.toDouble / coverPoints
      out.append(s""""cumulative_coverage": ${cumulativeCoverage.toString}""")

      out.append("}")

      if (cumulativeCoverage == 1.0) {
        println(s"""Cumulative coverage reached 100% early. Stopping on file: $input_name""")
        return
      }

      if (filesCovIter.hasNext) {
        out.append(", \n")
      }
    }
    out.append("\n]")
  }

  //Converts base AFL coverage (number of times each signal is on or off)
  //to toggle coverage (whether each signal has been both on and off)
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
