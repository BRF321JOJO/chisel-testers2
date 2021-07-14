/*
 * Copyright (c) 2017-2021 The Regents of the University of California
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


package chiseltest.fuzzing.afl

import chiseltest.fuzzing.{FuzzTarget, Rfuzz}

import java.io.{InputStream, OutputStream}

/** Provides a main function that can be used to interface with the AFL fuzzer.
 *
 *  Based on code written by Rohan Padhye and Caroline Lemieux for the JQF project
 * */
object AFLDriver extends App {
  def usage = "Usage: java " + this.getClass + " FIRRTL TEST_INPUT_FILE AFL_TO_JAVA_PIPE JAVA_TO_AFL_PIPE"
  require(args.length == 4, usage + "\nNOT: " + args.mkString(" "))

  val firrtlSrc = args(0)
  val inputFile = os.pwd / args(1)
  val (a2jPipe, j2aPipe) = (os.pwd / args(2), os.pwd / args(3))

  // load the fuzz target
  println(s"Loading and instrumenting $firrtlSrc...")
  val target = Rfuzz.firrtlToTarget(firrtlSrc, "test_run_dir/rfuzz_with_afl")
  println("Ready to fuzz! Waiting for someone to open the fifos!")
  AFLProxy.fuzz(target, a2jPipe, j2aPipe, inputFile)


}

/** Communicates with the AFLProxy written by Rohan Padhye and Caroline Lemieux for the JQF project */
object AFLProxy {
  val CoverageMapSize = 1 << 16
  def fuzz(target: FuzzTarget, a2jPipe: os.Path, j2aPipe: os.Path, inputFile: os.Path): Unit = {
    // connect to the afl proxy
    val proxyInput = os.read.inputStream(a2jPipe)
    val proxyOutput = os.write.outputStream(j2aPipe)

    // fuzz
    try {
      while (waitForAFL(proxyInput)) {
        // println("Waiting for input.")

        // println("Executing input.")
        val in = os.read.inputStream(inputFile)
        val (coverage, _) = target.run(in)
        in.close()
        // println(s"Sending coverage feedback. ($coverage)")
        handleResult(proxyOutput, coverage.toArray)
      }
    } catch {
      case _: java.io.IOException =>
    }

    target.finish(verbose = true)
  }

  private def waitForAFL(proxyInput: InputStream): Boolean = {
    // Get a 4-byte signal from AFL
    val signal = new Array[Byte](4)
    val received = proxyInput.read(signal, 0, 4)
    received == 4
  }

  private def handleResult(proxyOutput: OutputStream, coverage: Array[Byte]): Unit = {
    require(coverage.length < CoverageMapSize)
    val result = Result.Success // TODO
    val status = Result.toStatus(result)
    writeInt(proxyOutput, status)
    // indicate how many bytes we are going to send + 1
    writeInt(proxyOutput, coverage.length + 1)
    // send one dummy byte to avoid the "No instrumentation detected" error from AFL
    proxyOutput.write(1)
    // send actual coverage bytes
    proxyOutput.write(coverage)
    proxyOutput.flush()
  }

  private def writeInt(out: OutputStream, value: Int): Unit = {
    val buf = java.nio.ByteBuffer.allocate(4)
    buf.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    buf.putInt(value)
    out.write(buf.array())
  }
}

object Result extends Enumeration {
  val Success, Invalid, Failure, Timeout = Value
  def toStatus(v: Result.Value): Int = v match {
    case Success => 0
    case Invalid =>
      // For invalid inputs, we send a non-zero return status
      // in the second smallest byte, which is the program's return status
      // for programs that exit successfully
      1 << 8
    case Failure =>
      // For failure, the exit value is non-zero in LSB to simulate exit with signal
      6 // SIGABRT
    case Timeout =>
      // For timeouts, we mock AFL's behavior of having killed the target
      // with a SIGKILL signal
      9 // SIGKILL
  }
}
