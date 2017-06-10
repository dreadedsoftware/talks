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
  //   The canonical Mapping type thingy
  trait Mapping[A, B]{
    def map(a: A): B
  }
  val mapping: Mapping[List[Int], List[String]] =
    new Mapping[List[Int], List[String]]{
      override def map(a: List[Int]): List[String] =
        a.map(_.toString)
    }
  
  //3.1 Super restrictive, need new Mapping for list of any type
  //    We want to map any List but change the type within the List
  trait ListMapping[A, B]{
    def map(list: List[A])(f: A => B): List[B] = list.map(f)
  }
  
  //3.2 And a little shifting of type parameters we have
  object ListMapping2{
    def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)
  }
  //dropped type parameters from outside the trait into the trait
  //this provides more freedom at the call site
  //This is crazy! why not just call map on List?!?!?!
  //What of we are dealing with a type which doesn't have a map
  //or we need a different version of map, say reverse then map
  object ListReverseMapping{
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
  //   some kind of data zipper
  //   Say we have some number of pipelines and they each output
  //   a particular type of data. Now we want to make sure we
  //   correllate all the first elements, second elements and so on
  //   without loss of type information. Also we want to add, remove
  //   and rearrange pipelines without refactoring.
  //   Let's start with what we don't want and abstract from there
  def wayTooMuchMaintainance[A, B, C](
    a: List[A], b: List[B], c: List[C]
  ): List[(A, (B, C))] = a.zip(b.zip(c))
  
  //5.1 List seems unreasonably restrictive let's get rid of that
  trait Zip[F[_]]{
    def apply[A, B](a: F[A], b: F[B]): F[(A, B)]
  }
  implicit object ZipList extends Zip[List]{
    override def apply[A, B](
      a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
  }
  def tooMuchMaintainance[F[_]: Zip, A, B, C](
    a: F[A], b: F[B], c: F[C]): F[(A, (B, C))] = {
    val F = implicitly[Zip[F]]
    F(a, F(b, c))
  }
  
  //5.2 So, we can rearrange without refactoring but not change
  //    the arity of the function
  
  def rnd = scala.util.Random.nextInt()
}
