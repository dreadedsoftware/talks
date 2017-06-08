object TyDD{
  //1. This is a function
  //   keyword, delim, input, delim, output, body
  def f(a: Int, b: Int): String = {
    a.toString + b.toString
  }
  
  //2. This is a function at the type level
  //   keywords, delim, input, delim, output, body
  trait F[A, B]{type Out}
  object F2{
    def apply[A, B, C](): F[A, B]{type Out = C} =
      new F[A, B]{override type Out = C}
  }
  //2.1 To simplify we use the Aux pattern
  object F2_1{
    type Aux[A, B, C] = F[A, B]{type Out = C}
    def apply[A, B, C](): Aux[A, B, C] =
      new F[A, B]{override type Out = C}
  }
  
  //3. Higher Kinds
  //   The canonical Mapper type thingy
  trait Mapper[A, B]{
    def map(a: A): B
  }
  val mapper: Mapper[List[Int], List[String]] =
    new Mapper[List[Int], List[String]]{
      override def map(a: List[Int]): List[String] =
        a.map(_.toString)
    }
  
  //3.1 Super restrictive, need new Mapper for list of any type
  //    We want to map any List but change the type within the List
  trait ListMapper[A, B]{
    def map(list: List[A])(f: A => B): List[B] = list.map(f)
  }
  
  //3.2 And a little shifting of type parameters we have
  object ListMapper2{
    def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)
  }
  //dropped type parameters from outside the trait into the trait
  //this provides more freedom at the call site
  //This is crazy! why not just call map on List?!?!?!
  //What of we are dealing with a type which doesn't have a map
  //or we need a different version of map, say reverse then map
  object ListReverseMapper{
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.
        reverse.
        map(f)
  }
  //The power of this approach is being able to define more than
  //one map function for List without having to redefine List.
  
  //3.2 What if we want to map Option or Stream or any other type
  //    with type parameters? Scala offers a solution
  trait WithMap[F[_]]{
    def map[A, B](m: F[A])(f: A => B): F[B]
  }
  implicit val listWithMap = new WithMap[List]{
    override def map[A, B](m: List[A])(f: A => B): List[B] =
      m.map(f)
  }
  implicit val optionWithMap = new WithMap[Option]{
    override def map[A, B](m: Option[A])(f: A => B): Option[B] =
      m.map(f)
  }
  implicit val streamWithMap = new WithMap[Stream]{
    override def map[A, B](m: Stream[A])(f: A => B): Stream[B] =
      m.map(f)
  }
  val reverseListWithMap = new WithMap[List]{
    override def map[A, B](m: List[A])(f: A => B): List[B] =
      m.reverse.map(f)
  }
  //works with any type we can define a suitable function for
  
  //3.3 The point of Higher Kinds
  //    We can define uber polymorphic functions now
  def prettyString[F[_]: WithMap, A](m: F[A])(f: A => String): String = {
    implicitly[WithMap[F]].map(m)(f).toString
  }
  def processData[F[_]: WithMap, A, B, C, D](m1: F[A])(
    f1: A => B)(
    f2: B => C)(
    f3: C => D): F[D] = {
    val F = implicitly[WithMap[F]]
    val m2 = F.map(m1)(f1)
    val m3 = F.map(m2)(f2)
    F.map(m3)(f3)
  }
  //WithMap is what is known as a Functor (more precisely Endofunctor)
  //so whenever you see Functor, just think it has a map operation
  
  def ex3{
    val list = List(rnd, rnd, rnd, rnd, rnd)
    val stream = Stream(rnd, rnd, rnd, rnd, rnd)
    val option = Option(rnd)
    val f1: Int => String = {i: Int => "As String: " + i.toString}
    val f2: String => Array[Byte] = _.getBytes
    val f3: Array[Byte] => Long = _.map(_.toLong).sum
    
    println(prettyString(list)(f1))
    println(prettyString(option)(f1))
    println(prettyString(stream)(f1))
    println()
    println(processData(list)(f1)(f2)(f3))
    println(processData(option)(f1)(f2)(f3))
    println(processData(stream)(f1)(f2)(f3))
  }
  
  //4. Now that we can read Type functions we can fully utilize
  //   many libraries which employ them.
  //   The most obvious is shapeless.
  //   https://github.com/milessabin/shapeless#shapeless-232-with-sbt
  
  //4.1 HList
  val justAList = 1 :: 1.1 :: "1" :: Nil
  import shapeless._
  val anHList = 1 :: 1.1 :: "1" :: HNil
  
  //5. A real-world example
  //   some kind of data processor
  trait Input[F[_], A]{def apply(): F[A]}
  trait Process[F[_], A, B]{def apply(a: F[A]): F[B]}
  trait Qa[F[_], A, B, C]{
    def raw(in: F[A]): F[A]
    def process1(in: F[B]): F[B]
    def process2(in: F[C]): F[C]
  }
  trait Processor[A, B, C]{
    type Out
    def apply(): Out
  }
  //5.1 A basic implementation would look like this
  object Processor{
    type Aux[A, B, C, D] =
      Processor[A, B, C]{type Out = D}
    def apply[F[_], A, B, C](implicit
      input: Input[F, A],
      process1: Process[F, A, B],
      process2: Process[F, B, C],
      qa: Qa[F, A, B, C]
    ): Aux[A, B, C, F[C]] =
      new Processor[A, B, C]{
        override type Out = F[C]
        override def apply(): Out = {
          val raw = qa.raw(input())
          val p1 = qa.process1(process1(raw))
          val result = qa.process2(process2(p1))
          result
        }
      }
  }
  
  //6. What about more complex implementations
  //   Single input with multiple process steps
  //   In pseudocode Processor[A, B1 and B2 and B3, C1 and C2 and C3]
  //   The parts are
  import scala.util.Try
  implicit val input: Input[Try, Int] = new Input[Try, Int]{
    override def apply(): Try[Int] = Try{rnd}
  }
  
  implicit val p11: Process[Try, Int, Long] = 
    new Process[Try, Int, Long]{
      override def apply(in: Try[Int]): Try[Long] = in.
        filter(i => 0 == (i % 2)).map{i =>
          i.toLong * rnd
        }
    }
  implicit val p12: Process[Try, Int, Double] = 
    new Process[Try, Int, Double]{
      override def apply(in: Try[Int]): Try[Double] = in.
        filter(i => 1 == (i % 2)).map{i =>
          i.toDouble * rnd
        }
    }
  implicit val p13: Process[Try, Int, String] = 
    new Process[Try, Int, String]{
      override def apply(in: Try[Int]): Try[String] = in.map(_.toString)
    }
  //All together
  type P1 = Long :: Double :: String :: HNil
  
  implicit val p21: Process[Try, Long, Double] = 
    new Process[Try, Long, Double]{
      override def apply(in: Try[Long]): Try[Double] = in.map(_.toDouble)
    }
  implicit val p22: Process[Try, Double, String] = 
    new Process[Try, Double, String]{
      override def apply(in: Try[Double]): Try[String] = in.map(_.toString)
    }
  implicit val p23: Process[Try, String, Array[Byte]] = 
    new Process[Try, String, Array[Byte]]{
      override def apply(in: Try[String]): Try[Array[Byte]] = in.map(_.getBytes())
    }
  //All together
  type P2 = Double :: String :: Array[Byte] :: HNil
  
  implicit val qa1: Qa[Try, Int, Long, Double] =
    new Qa[Try, Int, Long, Double]{
      def raw(in: Try[Int]): Try[Int]= in
      def process1(in: Try[Long]): Try[Long] = in
      def process2(in: Try[Double]): Try[Double] = in
    }
  implicit val qa2: Qa[Try, Int, Double, String] =
    new Qa[Try, Int, Double, String]{
      def raw(in: Try[Int]): Try[Int]= in
      def process1(in: Try[Double]): Try[Double] = in
      def process2(in: Try[String]): Try[String] = in
    }
  implicit val qa3: Qa[Try, Int, String, Array[Byte]] =
    new Qa[Try, Int, String, Array[Byte]]{
      def raw(in: Try[Int]): Try[Int]= in
      def process1(in: Try[String]): Try[String] = in
      def process2(in: Try[Array[Byte]]): Try[Array[Byte]] = in
    }
  
  //6.1 The most basic implementation
  val processAllBasic = new Processor[Int, P1, P2]{
    type Out = Try[Double] :: Try[String] :: Try[Array[Byte]] :: HNil
    override def apply(): Out = {
      val in = input()
      val processed11 = p11(in)
      val processed12 = p12(in)
      val processed13 = p13(in)
      
      val processed21 = p21(processed11)
      val processed22 = p22(processed12)
      val processed23 = p23(processed13)
      
      processed21 :: processed22 :: processed23 :: HNil
    }
  }
  
  def rnd = scala.util.Random.nextInt()
}
