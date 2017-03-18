import java.math._

import shapeless._
import cats._

import java.net._
import java.io._
import Pipelines._

object RunPipeline extends App{
  def fibStr(uri: URI): Stream[String] = get(uri)
  def fib(uri: URI): Stream[BigInt] = fibStr(uri).map(strToBigInt)
  def triStr(uri: URI): Stream[String] = get(uri)
  def tri(uri: URI): Stream[BigInt] = triStr(uri).map(strToBigInt)
  def fac(uri: URI): Stream[String] = get(uri)
  
  def get(uri: URI): Stream[String] =
    scala.io.Source.fromFile(new File(uri)).getLines().toStream
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
    
    println("zeroth |" + result + "| = " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  def do1(){
    import _1._
    val pipeline = new Pipeline[Stream, BigInt, String]{
      override def read(uri: URI): Stream[BigInt] = fib(uri)
      override def computation(in: Stream[BigInt]): Stream[String] = in.map(_.toString)
      override def write(in: Stream[String]): Stream[Unit] = in.map(_=>())
    }
    
    val result = pipeline(uriFib)
    
    println("first |" + result + "| =  " + result.foldLeft(0)((a, b) => a + 1))
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
    println("second |" + result + "| =  " + result.foldLeft(0)((a, b) => a + 1))
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
    println("third |" + result + "| =  " + result.foldLeft(0)((a, b) => a + 1))
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
    implicit def readFac = new Read[Stream, String]{
      override def apply(uri: URI): Stream[String] = fac(uri)
    }
    implicit def compIS = new Computation[BigInt, String]{
      override def apply(in: BigInt): String = in.toString
    }
    implicit def compSI = new Computation[String, BigInt]{
      override def apply(in: String): BigInt = strToBigInt(in)
    }
    implicit def writeS = new Write[String]{
      override def apply(in: String): Unit = ()
    }
    implicit def writeI = new Write[BigInt]{
      override def apply(in: BigInt): Unit = ()
    }
  }
  def do4(){
    import implicits._
    import implicitFib._
    import _4._
    val pipeline = new Pipeline[Stream, BigInt, String]{}
    
    val result = pipeline(uriFib)
    println("fourth |" + result + "| =  " + result.foldLeft(0)((a, b) => a + 1))
  }
  
  def do5(){
    import implicits._
    import implicitFib._
    import _5._
    val pipeline = Pipeline[Stream, BigInt, String]
    val result = pipeline(uriFib)
    println("fifth |" + result + "| =  " + result.foldLeft(0)((a, b) => a + 1))
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
    val pipelineFac = Pipeline[Stream, String, BigInt]
    def perform(uri: URI): Stream[Unit] = {
      if(uri.toString.contains("fib")){
        pipelineFib(uri)
      } else if(uri.toString.contains("tri")){
        pipelineTri(uri)
      } else if(uri.toString.contains("fac")){
        pipelineFac(uri)
      } else Stream()
    }
    
    List(uriFib, uriTri, uriFac, uriDummy).map(perform).foreach{result =>
      println("sixth |" + result + "| =  " + result.foldLeft(0)((a, b) => a + 1))
    }
  }
  
  trait Fib
  trait Tri
  trait Fac
  implicit def guardFib = new Guard[Fib]{
    final override def name: String = "fib"
  }
  implicit def guardTri = new Guard[Tri]{
    final override def name: String = "tri"
  }
  implicit def guardFac = new Guard[Fac]{
    final override def name: String = "fac"
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
  implicit def pipelineFac: Pipeline.Aux[Fac, BigInt, String, Either[Unit, Stream[Unit]]] = {
    import implicits._
    import implicitTri._
    Pipeline[Fac, Stream, BigInt, String]
  }
  def do7(){
    val onSuccess = {result: Stream[Unit] =>
      println("seventh |" + result + "| =  " + result.foldLeft(0)((a, b) => a + 1))
    }
    val onFailure = {u: Unit =>
      println("seventh wrong pipe")
    }
    pipelineFib(uriFib).fold(onFailure, onSuccess)
    pipelineFib(uriTri).fold(onFailure, onSuccess)
    
    pipelineTri(uriFib).fold(onFailure, onSuccess)
    pipelineTri(uriTri).fold(onFailure, onSuccess)
    
    def perform(uri: URI): Either[Either[Either[Unit, Stream[Unit]], Stream[Unit]], Stream[Unit]] = {
      pipelineFib(uri).fold(
        _ => Left(pipelineTri(uri).fold(
          _ => Left(pipelineFac(uri).fold(
            _ => Left(()),
            a => Right(a)
          )),
          a => Right(a)
        )),
        a => Right(a)
      )
    }
    
    List(uriFib, uriTri).map(perform).foreach(println)
  }
  
  //incrementally make sure things work
  def do8(){
    import Pipelines.implicits._
    import shapeless._
    
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
    
    val pipeline = pipelineTri +: pipelineFac +: pipelineFib +: PNil
    
    List(uriFib, uriTri, uriDummy, uriFac).foreach{uri =>
      println("eighth " + pipeline(uri))
    }
  }
  
  def uriFib = new File("fibs").toURI
  def uriTri = new File("tris").toURI
  def uriFac = new File("facs").toURI
  def uriDummy = new File("dummy").toURI
}
