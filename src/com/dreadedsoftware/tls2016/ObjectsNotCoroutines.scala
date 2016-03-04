package com.dreadedsoftware.tls2016

import CaseClasses.SimpleSub._

object ObjectsNotCoroutines extends App{
  asCoroutine()
  
  
  final case class Location(where: String)
  val North = Location("North")
  val South = Location("South")
  val East = Location("East")
  val West = Location("West")
  
  final case class Depth(deep: String)
  val Shallow = Depth("Shallow")
  val Deep = Depth("Deep")
  
  import scala.collection.immutable
  case class School(
      name: String,
      depth: Depth,
      location: Location,
      fish: immutable.Queue[Fish])
  def aBetterWay(): Unit = {
    @annotation.tailrec
    def perform(qty: Int, acc: List[School]): List[School] = {
      if(qty > 0 && acc.nonEmpty){
        val head :: tail = acc
        val currentFish = head.fish.last
        val next = nextFish(currentFish)
        val result = head.copy(fish = head.fish.enqueue(next))
        perform(qty - 1, result :: acc)
      }else acc
    }
    
    val school = School(
        "Bikini Bottom",
        Deep,
        South,
        immutable.Queue(OneFish))
    val result = perform(10, List(school))
    result.foreach(convertToJsonAndPutOnTheWire)
  }
  
  def nextFish(current: Fish): Fish = {
    def fish(one: Float): Fish = {
      if(one < scala.util.Random.nextFloat()){
        OneFish
      }else TwoFish
    }
    current match{
      case OneFish => fish(.3f)
      case TwoFish => fish(.7f)
      case _ => fish(.5f)
    }
  }
  def convertToJsonAndPutOnTheWire(school: School): Unit = {
    println(school)
  }
  
  
  import scala.collection.mutable
  class BadSchool(){
    private var name: String = null
    private var depth: Depth = null
    private var location: Location = null
    private var fish: mutable.Buffer[Fish] = null
    
    def setName(newName: String): Unit = {
      name = newName
    }
    def getName(): String = name
    
    def setDepth(newDepth: Depth): Unit = {
      depth = newDepth
    }
    def getDepth(): Depth = depth
    
    def setLocation(newLocation: Location): Unit = {
      location = newLocation
    }
    def getLocation(): Location = location
    
    def setFish(newFish: mutable.Buffer[Fish]): Unit = {
      fish = newFish
    }
    def removeFish(aFish: Fish): Unit = {
      fish -= aFish
    }
    def addFish(aFish: Fish): Unit = {
      fish += aFish
    }
    def getFish(): mutable.Buffer[Fish] = fish
    
    override def toString(): String= {
      s"School(\n\t$name,\n\t$depth,\n\t$location,\n\t$fish)"
    }
  }
  
  def asCoroutine(): Unit = {
    val coroutine = new BadSchool()
    val (name, depth, location, fish) = someInit()
    coroutine.setName(name)
    coroutine.setDepth(depth)
    coroutine.setLocation(location)
    coroutine.setFish(fish)
    convertToJsonAndPutOnTheWire(coroutine)
    
    var newFish: Fish = null
    for(i <- (0 to 10)){
      newFish = nextFish(coroutine)
      coroutine.addFish(newFish)
      convertToJsonAndPutOnTheWire(coroutine)
    }
  }
  type InitType = (String, Depth, Location, mutable.Buffer[Fish])
  def someInit(): InitType = {
    ("blah", Deep, North, mutable.Buffer(OneFish))
  }
  def convertToJsonAndPutOnTheWire(school: BadSchool): Unit = {
    println(school)
  }
  def nextFish(school: BadSchool): Fish = {
    def fish(one: Float): Fish = {
      if(one < scala.util.Random.nextFloat()){
        OneFish
      }else TwoFish
    }
    school.getFish().last match{
      case OneFish => fish(.3f)
      case TwoFish => fish(.7f)
      case _ => fish(.5f)
    }
  }
}
