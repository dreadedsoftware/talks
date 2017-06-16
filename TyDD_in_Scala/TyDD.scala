object TyDD extends App{
  val list1 = List(rnd, rnd, rnd)
  val list2 = List(rnd, rnd, rnd).map(_.toLong * Int.MaxValue)
  val list3 = List(rnd, rnd, rnd).map(_.toString)
  val list4 = List(rnd, rnd, rnd).map(_.toDouble * 0.34)
  val list5 = List(rnd, rnd, rnd).map(_.toFloat * 0.34f)
  val list6 = List(rnd, rnd, rnd).map(_.toString.getBytes)
  
  val opt1 = list1.headOption
  val opt2 = list2.headOption
  val opt3 = list3.headOption
  val opt4 = list4.headOption
  val opt5 = list5.headOption
  val opt6 = list6.headOption 
  
  //1. This is a function
  //   keyword, input, output, body
  def f(a: Int, b: Int): String = {
    a.toString + b.toString
  }
  
  //2. This is a function at the type level
  //   keywords, input, output, body
  trait MyTrait[A, B]{type Out}
  object MyTrait{
    def apply[A, B, C](): MyTrait[A, B]{type Out = C} =
      new MyTrait[A, B]{override type Out = C}
  }
  //2.1 To simplify we use the Aux pattern
  object MyTrait1{
    type Aux[A, B, C] = MyTrait[A, B]{type Out = C}
    def apply[A, B, C](): Aux[A, B, C] =
      new MyTrait[A, B]{override type Out = C}
  }
  
  
  def ex3{
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
    trait ListMapping2{
      def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)
    }
    //dropped type parameters from outside the trait into the trait
    //this provides more freedom at the call site
    //This is crazy! why not just call map on List?!?!?!
    //What of we are dealing with a type which doesn't have a map
    //or we need a different version of map, say reverse then map
    object ListReverseMapping extends ListMapping2{
      override def map[A, B](list: List[A])(f: A => B): List[B] =
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
  
  type F[A] = List[A] //define F
  type Result1 =
    F[(Int, (Long, (String, (Double, (Float, Array[Byte])))))]//define result
  
  def ex4{
    //4. A real-world example
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
    def tooMuchMaintainance[F[_]: Zip, A, B, C](
      a: F[A], b: F[B], c: F[C]): F[(A, (B, C))] = {
      val F = implicitly[Zip[F]]
      F(a, F(b, c))
    }
    
    //4.2 So, we can rearrange without refactoring but not change
    //    the arity of the function. This is where we see the true
    //    power of a type level function. Since we can construct types
    //    we can "fake" a function which has arbitrary arity and
    //    arbitrary input types.
    def maintenance[F[_]: Zip, H, T](h: F[H], t: F[T]): F[(H, T)] = {
      val F = implicitly[Zip[F]]
      F(h, t)
    }
  
    implicit val ZipList = new Zip[List]{
      override def apply[A, B](
        a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
    }
    printType(
      maintenance(list1,
                  list2))
    printType(
      maintenance(list1,
      maintenance(list2,
                  list3))
    )
    printType(
      maintenance(list1,
      maintenance(list2,
      maintenance(list3,
                  list4)))
    )
    printType(
      maintenance(list1,
      maintenance(list2,
      maintenance(list3,
      maintenance(list4,
                  list5))))
    )
    printType(
      maintenance(list1,
      maintenance(list2,
      maintenance(list3,
      maintenance(list4,
      maintenance(list5,
                  list6)))))
    )
    //We can rearrange at will! But adding and removing is still
    //a bit too much for us. We would simply like to tell the
    //compiler
    //A. the type of F
    //B. the resulting type we want
    //C. the composite F values
    //Then the compiler can figure the rest out for us.
    
    //4.3 Implicit everything
    //    We already use implicits to bring in our type class instances
    //    We can also use them for plain old values
    implicit def zipper[F[_]: Zip, H, T](implicit
      h: F[H], t: F[T]): F[(H, T)] = {
      val F = implicitly[Zip[F]]
      F(h, t)
    }
    
    implicit val _1 = list1
    implicit val _2 = list2
    implicit val _3 = list3
    implicit val _4 = list4
    implicit val _5 = list5
    implicit val _6 = list6
    
    printType(implicitly[Result1])
  }
  
  //5. Why stop at tuples?
  //   What we are doing is combining two values into
  //   A type of those two values. Who cares what combined type
  //   the client code uses?
  trait ZipG[F[_], G[_, _]]{
    def apply[A, B](a: F[A], b: F[B]): F[G[A, B]]
  }
  
  //5.1 And now a function to describe
  //    If we follow the types we should be ok.
  implicit def zipperG[F[_], G[_, _], H, T](implicit
    F: ZipG[F, G], h: F[H], t: F[T]): F[G[H, T]] ={
    F(h, t)
  }
  val zipListTuple2 = new ZipG[List, Tuple2]{
    override def apply[A, B](
      a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
  }
  
  def ex5{
    implicit val zlt = zipListTuple2
    
    implicit val _1 = list1
    implicit val _2 = list2
    implicit val _3 = list3
    implicit val _4 = list4
    implicit val _5 = list5
    implicit val _6 = list6
    
    printType(implicitly[Result1])
    
    implicit val ZipListEither = new ZipG[List, Either]{
      override def apply[A, B](
        a: List[A], b: List[B]): List[Either[A, B]] =
        for{
          a <- a; b <- b
        }yield{
          if(a.toString.size < b.toString.size) Left(a)
          else Right(b)
        }
    }
    
    type Result2 =
      F[Either[Int, Either[Long, Either[String, Either[Double, Either[Float, Array[Byte]]]]]]]//define result
    printType(implicitly[Result2])
  }
  
  def ex6{
    //6. Processing the result
    //   We have a writer, now we need a reader
    //   We again begin with a naive example
    def stringify1[A, B, C](
      fa: A => String, fb: B => String, fc: C => String,
      in: List[(A, (B, C))]): String = {
      in.map{case (a, (b, c)) =>
        fa(a) + ", " + fb(b) + ", " + fc(c)
      }.mkString("(", "; ", ")")
    }
    
    //6.1 The first step here is to map over the List
    //    recall our friend the Functor. It allows us
    //    to say when a structure can be mapped over
    //    one is provided by the cats library
    import cats.Functor
    val functorList = new Functor[List]{
      override def map[A, B](fa: List[A])(f: A => B): F[B] = fa.map(f)
    }
    def stringify2[F[_]: Functor, A, B, C](
      fa: A => String, fb: B => String, fc: C => String,
      in: F[(A, (B, C))]): String = {
      val F = implicitly[Functor[F]]
      F.map(in){case (a, (b, c)) =>
        fa(a) + ", " + fb(b) + ", " + fc(c)
      }
      ???
    }
    //but now we need a mkString for our F
    //luckily cats provides one of these as well, called Show
    import cats.Show
    def stringify3[F[_]: Functor, A, B, C](
      fa: A => String, fb: B => String, fc: C => String,
      in: F[(A, (B, C))])(implicit
      FS: Show[F[String]]): String = {
      val F = implicitly[Functor[F]]
      val result = F.map(in){case (a, (b, c)) =>
        fa(a) + ", " + fb(b) + ", " + fc(c)
      }
      FS.show(result)
    }
    
    //6.2 No need to have these Function1 instances come in
    //    We can use Show for it all
    def stringify4[F[_]: Functor, A: Show, B: Show, C: Show](
      in: F[(A, (B, C))])(implicit
      FS: Show[F[String]]): String = {
      val F = implicitly[Functor[F]]
      val fa = implicitly[Show[A]].show _
      val fb = implicitly[Show[B]].show _
      val fc = implicitly[Show[C]].show _
      val result = F.map(in){case (a, (b, c)) =>
        fa(a) + ", " + fb(b) + ", " + fc(c)
      }
      FS.show(result)
    }
    
    //6.3 Now, we need a recursive version like before
    def stringify5[F[_]: Functor, A: Show, B: Show](
      in: F[(A, B)])(implicit
      FS: Show[F[String]]): String = {
      val F = implicitly[Functor[F]]
      val fa = implicitly[Show[A]].show _
      val fb = implicitly[Show[B]].show _
      val result = F.map(in){case (a, b) =>
        fa(a) + ", " + fb(b)
      }
      FS.show(result)
    }
    
    //6.4 This isn't what we want. The recursion is wholly 
    //    within another type! How can we get around this?
    //    The inner type is recursive and we only need to 
    //    provide a Show instance for it. Recurse on Show.
    implicit def makeShow[A: Show, B: Show]: Show[(A, B)] = {
      val fa = implicitly[Show[A]].show _
      val fb = implicitly[Show[B]].show _
      new Show[(A, B)]{
        override def show(t: (A, B)): String = {
          val (a, b) = t
          "(" + fa(a) + ", " + fb(b) + ")"
        }
      }
    }
    def stringify[F[_]: Functor, A: Show](
      in: F[A])(implicit
      FS: Show[F[String]]): String = {
      val F = implicitly[Functor[F]]
      val fa = implicitly[Show[A]].show _
      val result = F.map(in)(fa)

      FS.show(result)
    }
    
    implicit val ShowListString = new Show[List[String]]{
      def show(in: List[String]): String = in.mkString("(", "; ", ")")
    }
    
    //Functor instance
    implicit val listF = functorList
    implicit val zlt = zipListTuple2
    implicit val _1 = list1
    implicit val _2 = list2
    implicit val _3 = list3
    implicit val _4 = list4
    implicit val _5 = list5
    implicit val _6 = list6
    
    val result = implicitly[Result1]
    
    implicit val showInt = new Show[Int]{
      override def show(in: Int): String = in.toString
    }
    implicit val showLong = new Show[Long]{
      override def show(in: Long): String = in.toString
    }
    implicit val showString = new Show[String]{
      override def show(in: String): String = in
    }
    implicit val showDouble = new Show[Double]{
      override def show(in: Double): String = f"$in%.2f"
    }
    implicit val showFloat = new Show[Float]{
      override def show(in: Float): String = f"$in%.2f"
    }
    implicit val showArrayByte = new Show[Array[Byte]]{
      override def show(in: Array[Byte]): String = new String(in)
    }
    
    println(stringify(result))
  }
  
  def rnd = scala.util.Random.nextInt()
  def printType[T: Manifest](t: T): Unit = println(manOf(t))
  def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]
  
  def sep(int: Int) = int.toString + ("-" * 20)
  println(sep(1))
  println(sep(2))
  println(sep(3))
  ex3
  println(sep(4))
  ex4
  println(sep(5))
  ex5
  println(sep(6))
  ex6
}
