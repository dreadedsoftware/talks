import cats._
import java.net._

object Pipelines{
  //What is a pipeline?
  //0. Input -> Processing -> Output
  object _0{
    trait Pipeline[A, B]{
      final def apply(uri: URI): Stream[Unit] =
        write(computation(read(uri)))
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
      final def apply(uri: URI): F[Unit] =
        write(computation(read(uri)))
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
  //- Given a Product, Produce a Coproduct and an Output type
  //Shapeless  
  
  //7. We need a guard for the if; another Typeclass
  //7.1 We can fill it with some empty types and add it to our ctor
  trait Guard[-T]{
    def name: String
  }
  sealed trait Pipeline[-T, A, B]{
    type Out
    def apply(uri: URI): Out
  }
  object Pipeline{
    type Out[F[_]] = Either[Unit, F[Unit]]
    type Default[T, F[_], A, B] =
      Aux[T, A, B, Out[F]]
    type Aux[T, A, B, O] = Pipeline[T, A, B]{type Out = O}
    
    final def apply[T: Guard, F[_]: Functor, A, B](implicit
      read: Read[F, A],
      computation: Computation[A, B],
      write: Write[B]): Default[T, F, A, B] = {
      val G: Guard[T] = implicitly
      val F: Functor[F] = implicitly
      new Pipeline[T, A, B]{
        final override type Out = Pipeline.Out[F]
        override def apply(uri: URI): Out = {
          val from = uri.toString
          if(from.contains(G.name)) Right{
            val in = read(uri)
            val computed = F.map(in)(computation)
            F.map(computed)(write)
          } else Left(())
        }
      }
    }
  }
  //The Out pattern allows type level computations; functions of types
  //The Aux pattern gets around a compiler inconvenience
  
  //8. Products to Coproducts
  object implicits{
    import shapeless._
    import shapeless.ops.hlist._
 
    //pitfall!!! if this is a val Ops won't work
    implicit def PNil: Pipeline.Aux[CNil, CNil, CNil, Unit:+:CNil] = {
      new Pipeline[CNil, CNil, CNil]{
        type Out = Unit:+:CNil
        override def apply(uri: URI): Out = Inl(())
      }
    }
    
    implicit def inductivePipeline[
      TH, F[_], AH, BH,
      TT <: Coproduct, AT <: Coproduct, BT <: Coproduct, OT <: Coproduct
    ](implicit
      head: Pipeline.Aux[TH, AH, BH, Pipeline.Out[F]],
      tail: Pipeline.Aux[TT, AT, BT, OT]
    ): Pipeline.Aux[
      TH:+:TT,
      AH:+:AT, BH:+:BT,
      F[Unit]:+:OT
    ] = {
      new Pipeline[TH:+:TT, AH:+:AT, BH:+:BT]{
        final override type Out = F[Unit]:+:OT
        final override def apply(uri: URI): Out = {
          head(uri).fold(
            {_ =>
              Inr(tail(uri))
            },
            s => Inl(s)
          )
        }
      }
    }
    
    implicit class Ops[TT <: Coproduct,
      AT <: Coproduct, BT <: Coproduct, OT <: Coproduct
    ](
      val tail: Pipeline.Aux[TT, AT, BT, OT]
    ) extends AnyVal{
      def +:[TH, F[_], AH, BH](
        head: Pipeline.Aux[TH, AH, BH, Pipeline.Out[F]]
      ): Pipeline.Aux[TH:+:TT,AH:+:AT, BH:+:BT,F[Unit]:+:OT] =
        inductivePipeline[
          TH, F, AH, BH, TT, AT, BT, OT
        ](head, tail)
    }
  }
  
  object spark{
    trait BoundedFunctor[F[_], B[_]]{
      def map[U: B, V: B](fu: F[U])(f: U => V): F[V]
    }
    trait Functor[F[_]] extends BoundedFunctor[F, Id]
  }
}
