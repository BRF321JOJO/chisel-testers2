package chiseltest.fuzzing

object CoverageAnalysis extends App{

  def usage = "Usage: java " + this.getClass + " FIRRTL QUEUE COVERAGE_OUTPUT_FILE OPTIONAL: OVERWRITE_OUTPUT"
  require(args.length == 3 || args.length == 4, usage + "\nNOT: " + args.mkString(" "))

  val firrtlSrc = args(0)
  val queue = os.pwd/os.RelPath(args(1))
  val coverageOutputFile = os.pwd/args(2)

  //Handle potential issue with overwriting existing file with same name as output
  var overwriteOutput = false
  try {
    if (args.length == 4) {
      overwriteOutput = args(3).toBoolean
    }
  } catch {
    case _: Throwable => throw new Exception("OVERWRITE_OUTPUT arguement must be either: true/false")
  }
  if (!overwriteOutput && os.exists(coverageOutputFile)) {
    throw new Exception("OUTPUT FILE WOULD BE OVERWRITTEN. ADD true AS FOURTH ARGUMENT TO FORCE OVERWRITE")
  }
  //Generate blank output file
  os.write.over(coverageOutputFile, "")


  // load the fuzz target
  println(s"Loading and instrumenting $firrtlSrc...")
  val target = Rfuzz.firrtlToTarget(firrtlSrc, "test_run_dir/TLUL_with_afl")

  println("Generating coverage from input queue...")
  val files = os.list(queue).filter(os.isFile)
  //Counts of coverage for different mux lines
  val counts = files.map { inputFile =>
    val in = os.read.inputStream(inputFile)
    val coverage = target.run(in)
    in.close()
    coverage
  }

  println("Outputting cumulative coverage results to output file...")
  var overallCoverage = Set[Int]()

  os.write.append(coverageOutputFile, "[")
  files.zip(counts).foreach { case (file, count) =>
    overallCoverage = overallCoverage.union(processCoverage(count))
    val coverPoints = count.size/2
    val cumulativeCoverage = overallCoverage.size.toDouble / coverPoints

    os.write.append(coverageOutputFile, "{")
    os.write.append(coverageOutputFile, s""""filename": "${file.toString}", """)
    os.write.append(coverageOutputFile, s""""creation_time": ${os.mtime(file).toString}, """)
    os.write.append(coverageOutputFile, s""""cumulative_coverage": ${cumulativeCoverage.toString}""")
    os.write.append(coverageOutputFile, "}, \n")
  }
  os.truncate(coverageOutputFile, os.size(coverageOutputFile) - 3)
  os.write.append(coverageOutputFile, "]")

  println("Done!")


  //METHODS
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


