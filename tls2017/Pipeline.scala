import java.math._

import shapeless._
import cats._

import java.net._
import java.io._
import Pipelines._

object RunPipeline extends App{
  def fibStr(uri: URI): Stream[String] =
    scala.io.Source.fromFile(new File(uri)).getLines().toStream
  def fib(uri: URI): Stream[BigInt] = fibStr(uri).map(strToBigInt)
  def triStr(uri: URI): Stream[String] =
    scala.io.Source.fromFile(new File(uri)).getLines().toStream
  def tri(uri: URI): Stream[BigInt] = fibStr(uri).map(strToBigInt)
  
  def strToBigInt(str: String): BigInt = {
    new BigInteger(str)
  }
  
  do0()
  do1()
  do2()
  do3()
  do4()
  do5()
  do6()
  
  def do0(){
    import _0._
    val pipeline = new Pipeline[BigInt, String]{
      override def read(uri: URI): Stream[BigInt] = fib(uri)
      override def computation(in: Stream[BigInt]): Stream[String] = in.map(_.toString)
      override def write(in: Stream[String]): Stream[Unit] = in.map(_=>())
    }
    
    val result = pipeline(uriFib)
    
    println("zeroth " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  def do1(){
    import _1._
    val pipeline = new Pipeline[Stream, BigInt, String]{
      override def read(uri: URI): Stream[BigInt] = fib(uri)
      override def computation(in: Stream[BigInt]): Stream[String] = in.map(_.toString)
      override def write(in: Stream[String]): Stream[Unit] = in.map(_=>())
    }
    
    val result = pipeline(uriFib)
    
    println("first " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  def do2(){
    import _2._
    val pipeline = new Pipeline[Stream, BigInt, String]{
      override def read(uri: URI): Stream[BigInt] = fib(uri)
      override def computation(in: BigInt): String = in.toString
      override def write(in: String): Unit = ()
      override def convert[U, V](in: Stream[U])(f: U => V): Stream[V] = {
        in.map(f)
      }
    }
    
    val result = pipeline(uriFib)
    println("second " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  implicit def streamFunctor = new Functor[Stream]{
    override def map[A, B](fa: Stream[A])(f: A => B): Stream[B] = {
      fa.map(f)
    }
  }
  def do3(){
    import _3._
    val pipeline = new Pipeline[Stream, BigInt, String]{
      override def read(uri: URI): Stream[BigInt] = fib(uri)
      override def computation(in: BigInt): String = in.toString
      override def write(in: String): Unit = ()
    }
    
    val result = pipeline(uriFib)
    println("third " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  implicit def readFib = new Read[Stream, BigInt]{
    override def apply(uri: URI): Stream[BigInt] = fib(uri)
  }
  implicit def compIS = new Computation[BigInt, String]{
    override def apply(in: BigInt): String = in.toString
  }
  implicit def writeS = new Write[String]{
    override def apply(in: String): Unit = ()
  }
  def do4(){
    import _4._
    val pipeline = new Pipeline[Stream, BigInt, String]{}
    
    val result = pipeline(uriFib)
    println("fourth " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  def do5(){
    import _5._
    val pipeline = Pipeline[Stream, BigInt, String]
    val result = pipeline(uriFib)
    println("fifth " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  def do6(){
    import _6._
    implicit def readTri = new Read[Stream, BigInt]{
      override def apply(uri: URI): Stream[BigInt] = tri(uri)
    }
    val pipelineFib = Pipeline[Stream, BigInt, String](
      implicitly, readFib, implicitly, implicitly
    )
    val pipelineTri = Pipeline[Stream, BigInt, String](
      implicitly, readTri, implicitly, implicitly
    )
    def perform(uri: URI): Stream[Unit] = {
      if(uri.toString.contains("fib")){
        pipelineFib(uri)
      }else if(uri.toString.contains("tri")){
        pipelineTri(uri)
      }else Stream()
    }
    
    List(uriFib, uriTri).map(perform).foreach{result =>
      println("sixth " + result.foldLeft(0)((a, b) => a + 1))
    }
  }
  
  def uriFib = new File("fibs").toURI
  def uriTri = new File("tris").toURI
}
