object testData{
  import java.io._
  import java.math._
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  def apply(): Unit = {
    val fibf = Future{fib()}
    val trif = Future{tri()}
    val facf = Future{fac()}
    val dummyf = Future{dummy()}
    
    Await.result(fibf.zip(trif).zip(facf).zip(dummyf), duration.Duration.Inf)
  }
  @annotation.tailrec
  def write(writer: PrintWriter, values: Stream[BigInt], count: Int): Unit = {
    values match{
      case head #:: tail if(count > -1) =>
        writer.println(head)
        println(s"($count, $head)")
        write(writer, tail, count - 1)
      case _ =>
        writer.flush()
        writer.close()
    }
  }
  def fib(): Unit = {
    val out = new PrintWriter("fibs")
    out.println(0)
    out.println(1)
    var previous: BigInt = BigInteger.valueOf(0)
    var current: BigInt = BigInteger.valueOf(1)
    (2 to 500).foreach{_ =>
      val next = previous + current
      out.println(next)
      previous = current
      current = next
    }
    out.flush()
    out.close()
  }
  def tri(): Unit = {
    val out = new PrintWriter("tris")
    out.println(0)
    var current: BigInt = BigInteger.valueOf(0)
    (1 to 500).foreach{add =>
      val next = current + add
      out.println(next)
      current = next
    }
    out.flush()
    out.close()
  }
  def fac(): Unit = {
    val out = new PrintWriter("facs")
    out.println(1)
    var current: BigInt = BigInteger.valueOf(1)
    (2 to 501).foreach{mult =>
      val next = current * mult
      out.println(next)
      current = next
    }
    out.flush()
    out.close()
  }
  
  def dummy(): Unit = {
    val out = new PrintWriter("dummy")
    out.flush()
    out.close()
  }
}
