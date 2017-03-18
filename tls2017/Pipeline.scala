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
  do7()
  do8()
  
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
  
  object implicitFib{
    implicit def readFib = new Read[Stream, BigInt]{
      override def apply(uri: URI): Stream[BigInt] = fib(uri)
    }
  }
  object implicitTri{
    implicit def readTri = new Read[Stream, BigInt]{
      override def apply(uri: URI): Stream[BigInt] = tri(uri)
    }
  }
  object implicits{
    implicit def compIS = new Computation[BigInt, String]{
      override def apply(in: BigInt): String = in.toString
    }
    implicit def writeS = new Write[String]{
      override def apply(in: String): Unit = ()
    }
  }
  def do4(){
    import implicits._
    import implicitFib._
    import _4._
    val pipeline = new Pipeline[Stream, BigInt, String]{}
    
    val result = pipeline(uriFib)
    println("fourth " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  def do5(){
    import implicits._
    import implicitFib._
    import _5._
    val pipeline = Pipeline[Stream, BigInt, String]
    val result = pipeline(uriFib)
    println("fifth " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  def do6(){
    import implicits._
    import implicitFib._
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
  
  trait Fib
  trait Tri
  implicit def guardFib = new Guard[Fib]{
    final override def name: String = "fib"
  }
  implicit def guardTri = new Guard[Tri]{
    final override def name: String = "tri"
  }
  
  implicit def pipelineFib: Pipeline.Aux[Fib, BigInt, String, Either[Unit, Stream[Unit]]] = {
    import implicits._
    import implicitFib._
    Pipeline[Fib, Stream, BigInt, String]
  }
  implicit def pipelineTri: Pipeline.Aux[Tri, BigInt, String, Either[Unit, Stream[Unit]]] = {
    import implicits._
    import implicitTri._
    Pipeline[Tri, Stream, BigInt, String]
  }
  def do7(){
    val onSuccess = {result: Stream[Unit] =>
      println("seventh " + result.foldLeft(0)((a, b) => a + 1))
    }
    val onFailure = {u: Unit =>
      println("seventh wrong pipe")
    }
    pipelineFib(uriFib).fold(onFailure, onSuccess)
    pipelineFib(uriTri).fold(onFailure, onSuccess)
    
    pipelineTri(uriFib).fold(onFailure, onSuccess)
    pipelineTri(uriTri).fold(onFailure, onSuccess)
    
    def perform(uri: URI): Either[Either[Unit, Stream[Unit]], Stream[Unit]] = {
      pipelineFib(uri).fold(
        _ => Left(pipelineTri(uri).fold(
          _ => Left(()),
          a => Right(a)
        )),
        a => Right(a)
      )
    }
    
    List(uriFib, uriTri).map(perform).foreach{
      case Left(Left(())) =>
        println("seventh ()")
      case Left(Right(result)) =>
        println("seventh " + result.foldLeft(0)((a, b) => a + 1))
      case Right(result) =>
        println("seventh " + result.foldLeft(0)((a, b) => a + 1))
    }
  }
  
  //incrementally make sure things compile
  def do8(){
    import Pipelines.implicits._
    import shapeless._
    //implicit val pipeline1 = pipelineFib :: HNil
    //implicit val pipeline2 = pipelineTri :: HNil
    //implicit val pipeline3 = pipelineTri :: pipelineFib :: HNil
    
    {
      val a: Pipeline.Aux[CNil, CNil, CNil, Unit:+:CNil] = implicitly
    }
    
    {
      val a: Pipeline.Aux[
        Fib :+: CNil,
        BigInt :+: CNil,
        String :+: CNil,
        Stream[Unit] :+: Unit :+: CNil
      ] = inductivePipeline[
        Fib, Stream, BigInt, String,
        CNil, CNil, CNil, Unit :+: CNil
      ](pipelineFib, implicitly[Pipeline.Aux[CNil, CNil, CNil, Unit:+:CNil]])
    }
    
    {
      val a: Pipeline.Aux[
        Fib :+: CNil,
        BigInt :+: CNil,
        String :+: CNil,
        Stream[Unit] :+: Unit :+: CNil
      ] = inductivePipeline[
        Fib, Stream, BigInt, String,
        CNil, CNil, CNil, Unit :+: CNil
      ]
    }
    
    {
      val a: Pipeline.Aux[
        Fib :+: CNil,
        BigInt :+: CNil,
        String :+: CNil,
        Stream[Unit] :+: Unit :+: CNil
      ] = inductivePipeline
    }
    
    {
      val a = inductivePipeline[
        Fib, Stream, BigInt, String,
        CNil, CNil, CNil, Unit :+: CNil
      ]
    }
    
    {
      val a: Pipeline.Aux[
        Fib :+: CNil,
        BigInt :+: CNil,
        String :+: CNil,
        Stream[Unit] :+: Unit :+: CNil
      ] = implicitly
    }
    
    {
      val a: Pipeline.Aux[
        Tri :+: Fib :+: CNil,
        BigInt :+: BigInt :+: CNil,
        String :+: String :+: CNil,
        Stream[Unit] :+: Stream[Unit] :+: Unit :+: CNil
      ] = implicitly
    }
    
    {
      new Ops(PNil)
    }
    
    {
      new Ops(PNil).+:(pipelineFib)
    }
    
    {
      pipelineFib +: PNil
    }
    
    val pipeline = pipelineTri +: pipelineFib +: PNil
    
    List(uriFib, uriTri, uriDummy).foreach{uri =>
      println("eighth " + pipeline(uri))
    }
  }
  
  def uriFib = new File("fibs").toURI
  def uriTri = new File("tris").toURI
  def uriDummy = new File("dummy").toURI
}
