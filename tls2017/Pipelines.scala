import cats._
import java.net._

object Pipelines{
  //What is a pipeline?
  //0. Input -> Processing -> Output
  object _0{
    trait Pipeline[A, B]{
      final def apply(uri: URI): Stream[Unit] = write(computation(read(uri)))
      def read(uri: URI): Stream[A]
      def computation(in: Stream[A]): Stream[B]
      def write(in: Stream[B]): Stream[Unit]//in effect, no leaving!
    }
  }
  //This gets us abstraction over the data types being processed
  //Locked into our type constructor
  
  //1. But this is bound to Stream! No good!
  //1.1 Has Effects
  //1.2 Performs computation
  object _1{
    trait Pipeline[F[_], A, B]{
      final def apply(uri: URI): F[Unit] = write(computation(read(uri)))
      def read(uri: URI): F[A]
      def computation(in: F[A]): F[B]
      def write(in: F[B]): F[Unit]
    }
  }
  
  //2. Taking erasure into account, functions are the same
  //2.1 We can abstract this out even further!
  object _2{
    trait Pipeline[F[_], A, B]{
      final def apply(uri: URI): F[Unit] = {
        val in = read(uri)
        val computed = convert(in)(computation)
        convert(computed)(write)
      }
      def read(uri: URI): F[A]
      def computation(in: A): B
      def write(in: B): Unit
      def convert[U, V](in: F[U])(f: U => V): F[V]
    }
  }
  
  //3. This looks a lot like functors
  //3.1 Let's abstract F even further out
  object _3{
    trait Pipeline[F[_], A, B]{
      final def apply(uri: URI)(implicit F: Functor[F]): F[Unit] = {
        val in = read(uri)
        val computed = F.map(in)(computation)
        F.map(computed)(write)
      }
      def read(uri: URI): F[A]
      def computation(in: A): B
      def write(in: B): Unit
    }
  }
  //Why is the compiler able to find both F and F?
  //We are developing in two spaces, values and types!
  //Something profound here; lifted an implementation detail outside the class
  //Now multiple pipelines can use that same implementation detail
  //The processes of abstraction and decoupling are deeply intertwined
  //Typeclasses are mad baller

  //4. We can decouple more implementation details
  trait Read[F[_], A] extends Function1[URI, F[A]]
  trait Computation[A, B] extends Function1[A, B]
  trait Write[B] extends Function1[B, Unit]
  object _4{
    trait Pipeline[F[_], A, B]{
      final def apply(uri: URI)(implicit
        F: Functor[F],
        read: Read[F, A],
        computation: Computation[A, B],
        write: Write[B]): F[Unit] = {
        val in = read(uri)
        val computed = F.map(in)(computation)
        F.map(computed)(write)
      }
    }
  }
  //Whoa! Client code got more complex! Or did it?
  //We can use the same write for different computations or vice versa
  //Each independent part of the application can be truely independent
  //This gives us a freedom that's impossible to attain with subclasses
  //But we're still not done... FP libs should be composable!!!
  
  //5. Producing apply outside of the class
  trait __5{
    sealed trait Pipeline[F[_], A, B]{
      def apply(uri: URI): F[Unit]
    }
    object Pipeline{
      final def apply[F[_]: Functor, A, B](implicit
        read: Read[F, A],
        computation: Computation[A, B],
        write: Write[B]) = {
        val F: Functor[F] = implicitly
        new Pipeline[F, A, B]{
          override def apply(uri: URI): F[Unit] = {
            val in = read(uri)
            val computed = F.map(in)(computation)
            F.map(computed)(write)
          }
        }
      }
    }
  }
  object _5 extends __5
  //This cleaned up some client code, no more new
  //This also opens up the door to more constructors; key composition
  
  //6. uniform input; this is important! If, elseIf
  object _6 extends __5
  //This is not what we want client code to look like
  //Let's take a look at what we've done
  //- Given an ordered extensional set of Pipelines
  //- Produced a computation that is *at most 1* Pipeline
  //- Given a Product, Produce a Coproduct
  //Shapeless  
  
  //7. We need a guard for the if; another Typeclass
  //7.1 We can fill it with some empty types and add it to our ctor
  trait Guard[T]{
    def name: String
  }
  sealed trait Pipeline[T, F[_], A, B]{
    def apply(uri: URI): F[Unit]
  }
  object Pipeline{
    final def apply[T: Guard, F[_]: Functor, A, B](implicit
      guard: Guard[T],
      read: Read[F, A],
      computation: Computation[A, B],
      write: Write[B]) = {
      val F: Functor[F] = implicitly
      new Pipeline[T, F, A, B]{
        override def apply(uri: URI): F[Unit] = {
          val in = read(uri)
          val computed = F.map(in)(computation)
          F.map(computed)(write)
        }
      }
    }
  }
  
  
  //8. Products to Coproducts
  
}
