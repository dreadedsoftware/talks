import java.sql.Timestamp
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import akka.actor._

object factorial{
  def simple(n: Int): BigInt = {
    @annotation.tailrec
    def recurse(n: Int, acc: BigInt): BigInt = {
      if(1 < n) recurse(n - 1, n * acc)
      else acc
    }
    
    val start = System.currentTimeMillis
    val result = recurse(n, 1)
    val end = System.currentTimeMillis
    println("Took: " + (end - start) + "ms")
    result
  }
  
  def naiveAsync(n: Int)(implicit ec: ExecutionContext): Future[BigInt] = {
    Future{simple(n)}
  }
  
  def partialFactorial(low: Int, high: Int): BigInt = {
    @annotation.tailrec
    def recurse(low: Int, high: Int, acc: BigInt): BigInt = {
      if(low <= high) recurse(low, high - 1, high * acc)
      else acc
    }
    recurse(low, high, 1)
  }
  
  val identity: BigInt = 1
  def sync(n: Int): BigInt = {
    val start = System.currentTimeMillis
    val result = getBuckets(n).foldLeft(identity){(acc, bounds) =>
      partialFactorial(bounds._1, bounds._2) * acc
    }
    val end = System.currentTimeMillis
    println("Took: " + (end - start) + "ms")
    
    result
  }
  
  def async1(n: Int)(implicit ec: ExecutionContext): Future[BigInt] = {
    val buckets: Seq[(Int, Int)] = getBuckets(n)
    buckets.foldLeft(???){(acc: Future[BigInt], bounds) =>
      ???
    }
  }
  def async2(n: Int)(implicit ec: ExecutionContext): Future[BigInt] = {
    val identityF = Future.successful(identity)
    val buckets: Seq[(Int, Int)] = getBuckets(n)
    val start = System.currentTimeMillis
    val result = buckets.foldLeft(identityF){(acc, n) =>
      for{
        a <- acc
        b <- Future{partialFactorial(n._1, n._2)}
      }yield{a * b}
    }
    
    result.onComplete{_ =>
      val end = System.currentTimeMillis
      println("Took: " + (end - start) + "ms")
    }
    result
  }
  def async3(n: Int)(implicit ec: ExecutionContext): Future[BigInt] = {
    val identityF = Future.successful(identity)
    val buckets: Seq[(Int, Int)] = getBuckets(n)
    val start = System.currentTimeMillis
    val result = buckets.foldLeft(identityF){(acc, n) =>
      val next = Future{partialFactorial(n._1, n._2)}
      for{
        a <- acc
        b <- next
      }yield{a * b}
    }
    
    result.onComplete{_ =>
      val end = System.currentTimeMillis
      println("Took: " + (end - start) + "ms")
    }
    result
  }
  
  def async(n: Int)(implicit ec: ExecutionContext): Future[BigInt] = {
    val start = System.currentTimeMillis
    val id = Future.successful(identity)
    val buckets: Seq[(Int, Int)] = getBuckets(n)
    val partials: Seq[Future[BigInt]] = buckets.map{
      case (low, high) =>
        Future{partialFactorial(low, high)}
    }
    
    val result: Future[BigInt] =
      bigFuture(partials)
    result.onComplete{_ =>
      val end = System.currentTimeMillis
      println("Took: " + (end - start) + "ms")
    }
    result
  }
  
  @annotation.tailrec
  def bigFuture(s: Seq[Future[BigInt]])(implicit ec: ExecutionContext): Future[BigInt] = {
    val id = Future.successful(identity)
    if(1 < s.size){
      bigFuture(collapse(s))
    }else if(1 == s.size){
      s.head
    }else id
  }
  
  def collapse(seq: Seq[Future[BigInt]])(implicit ec: ExecutionContext): Seq[Future[BigInt]] = {
    val grouped: Seq[Seq[Future[BigInt]]] = seq.grouped(2).toSeq
    grouped.map{seq: Seq[Future[BigInt]] =>
      val fut: Future[Seq[BigInt]] = Future.sequence(seq)
      fut.map{seq =>
        seq.tail.fold(seq.head)((a, b) => a*b)
      }
    }
  }
}

object combinations{
  //n! / (k!(n-k)!)
  import scala.util.{Try, Success, Failure}
  
  case class Numerator(`n!`: BigInt)
  case class Denom(`k!(n-k)!`: BigInt)
  case class DenomLeft(`k!`: BigInt)
  case class DenomRight(`(n-k)!`: BigInt)
  
  class Combinations extends Actor{
    implicit val ec: ExecutionContext = context.dispatcher
    private var start: Long = _
    override def postStop() = {
      val end = System.currentTimeMillis
      println("  Took: " + (end - start) + "ms")
    }
    override def receive: Receive = {
      case (n: Int, k: Int) =>
        start = System.currentTimeMillis
        val sentBy = sender()
        val diff = n - k
        if(n < 1) nothingToDo(sentBy)
        else{
          perform(n, k, sentBy)
          context.become(compute(sentBy))
        }
      case message @ _ => //failure
        println(message)
        context.stop(self)
    }
    
    private var numerator: Option[BigInt] = None
    private var denominator: Option[BigInt] = None
    private def compute(sentBy: ActorRef): Receive = {
      case Numerator(n) => denominator.fold{
        numerator = Some(n)
      }{denominator =>
        sentBy ! (n / denominator)
        context.stop(self)
      }
      case Denom(d) => numerator.fold{
        denominator = Some(d)
      }{numerator =>
        sentBy ! (numerator / d)
        context.stop(self)
      }
      case message @ _ => //failure
        println(message)
        context.stop(self)
    }
    
    private def perform(n: Int, k: Int, sentBy: ActorRef) = {
      val `n-k` = n - k
      if(`n-k` < 0){
        nothingToDo(sentBy)
      }else{
        doFactorial(n)(self ! Numerator(_))(self ! _.toString())
        context.actorOf(
          Props(
            new Denominator
          ), "Denominator"
        ) ! (k, `n-k`)
      }
    }
    private def nothingToDo(sentBy: ActorRef) = sentBy ! (0: BigInt)
  }
  private class Denominator extends Actor{
    implicit val ec: ExecutionContext = context.dispatcher
    
    override def receive: Receive = {
      case (k: Int, diff: Int) =>
        val `n-k` = diff
        val sentBy = sender()
        context.actorOf(
          Props(
            new `k!`
          ), "KFactorial"
        ) ! k
        context.actorOf(
          Props(
            new `(n-k)!`
          ), "NDiffKFactorial"
        ) ! `n-k`
        
        context.become(hasNone(sentBy))
      case message @ _ => context.parent ! message
    }
    
    private def hasNone(sentBy: ActorRef): Receive = {
      case DenomLeft(a) =>
        context.become(hasLeft(a, sentBy))
      case DenomRight(b) =>
        context.become(hasRight(b, sentBy))
      case message @ _ =>
        context.parent ! message
        context.stop(self)
    }
    
    private def hasLeft(a: BigInt, sentBy: ActorRef): Receive = {
      case DenomRight(b) =>
        sentBy ! Denom(a * b)
      case message @ _ =>
        context.parent ! message
        context.stop(self)
    }
    
    private def hasRight(b: BigInt, sentBy: ActorRef): Receive = {
      case DenomLeft(a) =>
        sentBy ! Denom(a * b)
      case message @ _ =>
        context.parent ! message
        context.stop(self)
    }
  }
  
  private class `k!` extends Actor{
    implicit val ec: ExecutionContext = context.dispatcher
    
    override def receive: Receive = {
      case k: Int =>
        val sentBy = sender()
        val fact = doFactorial(k)(sentBy ! DenomLeft(_))(sentBy ! _)
        fact.onComplete(_ => context.stop(self))
    }
  }
  private class `(n-k)!` extends Actor{
    implicit val ec: ExecutionContext = context.dispatcher
    
    override def receive: Receive = {
      case diff: Int =>
        val `n-k` = diff
        val sentBy = sender()
        val fact = doFactorial(`n-k`)(sentBy ! DenomRight(_))(sentBy ! _)
        fact.onComplete(_ => context.stop(self))
    }
  }
  
  private def doFactorial(n: Int)
  (onSuccess: BigInt => Unit)
  (onFailure: Throwable => Unit)
  (implicit ec: ExecutionContext): Future[BigInt] = {
    factorial.async(n).andThen{
      case Success(value) => onSuccess(value)
      case Failure(th) => onFailure(th)
    }
  }
}

object testActor{
  val system = ActorSystem("Asynchronicity" + System.currentTimeMillis)
  def apply(): ActorRef = {
    system.actorOf(Props(new combinations.Combinations), "Combo" + System.currentTimeMillis)
  }
  def down(): Unit = system.shutdown
}

object shutdown{def apply() = testActor.down()}

object test{
  import akka.pattern.ask
  import akka.util.Timeout
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val timeout = Timeout(1 hour)
  implicit class SuperInt(val n: Int) extends AnyVal{
    def choose(k: Int): Future[BigInt] = {
      val result = (testActor() ? (n, k)).
        collect{case n: BigInt => n}
      result.onComplete(println(_))
      result
    }
  }
}

object getBuckets{
  def apply(n: Int): Seq[(Int, Int)] = {
    val by10k = Stream.from(0, 10000)
    val needed = by10k.takeWhile(_ < n) :+ n
    if(needed.isEmpty) Seq((1, 1))
    else if(1 == needed.size) Seq((1, n))
    else{
      val zipped = needed.zipWithIndex
      val stream = zipped.tail.map{
        case (item, idx) => (needed(idx-1) + 1, item)
      }
      
      stream.toSeq
    }
  }
}
//(2016-10-02 23:32:00.608,2016-10-02 23:32:26.723)
//(2016-10-02 23:33:03.089,2016-10-02 23:33:24.658)