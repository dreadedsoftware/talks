package com.dreadedsoftware.tls2016

import argonaut._
import Argonaut._
import CaseClasses.SimpleSub._
import scala.collection.immutable.Queue
import monocle._
import monocle.macros.GenLens
import akka.actor.ActorSystem
import scala.concurrent.Future
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Actor
import akka.pattern.ask
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executor
import java.util.concurrent.Executors
import scala.collection.mutable

object MonocleAndArgonaut extends App{
  
  import scala.language.higherKinds
  case class Color(r: Byte, g: Byte, b: Byte)
  case class FishTank(liters: Int, color: Color, fish: List[Fish])
  
  val (tankLiters, tankColor, tankFish) = {
    val gen = GenLens[FishTank]
    (gen(_.liters), gen(_.color), gen(_.fish))
  }
  val (colorR, colorG, colorB) = {
    val gen = GenLens[Color]
    (gen(_.r), gen(_.g), gen(_.b))
  }
  val (tankColorR, tankColorG, tankColorB) = (
      tankColor.composeLens(colorR),
      tankColor.composeLens(colorG),
      tankColor.composeLens(colorB)
  )
  
  implicit def codecTank: CodecJson[FishTank] =
    casecodec3(
        FishTank.apply, FishTank.unapply
    )("liters", "color", "fish")
  implicit def codecColor: CodecJson[Color] =
    casecodec3(
        Color.apply, Color.unapply
    )("r", "g", "b")
  implicit def codecFish: CodecJson[Fish] =
    CodecJson(
      (f: Fish) =>
        ("name" := f.name) ->:
        ("color" := f.color) ->:
        jEmptyObject,
      (c: HCursor) => for{
        name <- (c --\ "name").as[String]
        color <- (c --\ "color").as[String]
      }yield{(name, color) match{
        case ("One Fish", "Red Fish") => OneFish
        case ("Two Fish", "Blue Fish") => TwoFish
        case _ => NotFish
      }}
    )
    
  val tanks = Map(
      1 -> FishTank(
          2,
          Color(0x77, 0x77, 0x77),
          List(OneFish)),
      2 -> FishTank(
          5,
          Color(0x77, 0x00, 0x00),
          List(OneFish, TwoFish, TwoFish))
  )
  
  println(tanks)
  val newTanks = tanks.map{
    case (int, tank) => (int,
      tankColor.modify { color =>
        Color(color.g, color.b, color.r)
      }(tank))
  }
  println(newTanks)
    
  object settings{
    private val settings: mutable.Map[String, FishTank] =
      mutable.Map()
    
    def apply(key: String): Option[FishTank] = settings.get(key)
    def update(key: String, byte: Byte): Unit = {
      settings(key) = settings.get(key) match{
        case Some(tank) =>
          tankColor.modify { _ => Color(byte, byte, byte) }(tank)
        case None =>
          FishTank(0, Color(byte, byte, byte), Nil)
      }
    }
    def update(key: String, size: Int): Unit = {
      settings(key) = settings.get(key) match{
        case Some(tank) =>
          tankLiters.modify(_ => size)(tank)
        case _ =>
          FishTank(size, Color(0,0,0), Nil)
      }
    }
    def update(key: String, fish:List[Fish]): Unit = {
      settings(key) = settings.get(key) match{
        case Some(tank) =>
          tankFish.modify(_ => fish)(tank)
        case _ =>
          FishTank(1, Color(0,0,0), fish)
      }
    }
    
    def persist(): Unit = {
      val jsonRaw = settings.toList.asJson
      val json = jsonRaw.nospaces
      putOnWire(json)
      writeToDisk(json)
    }
    def recall(): Unit = {
      val str = getFromDisk()
      val opt = str.decodeOption[List[(String, FishTank)]]
      opt.foreach{list =>
        settings ++= list.toMap
      }
    }
  }
  
  def putOnWire(str:String) = println(s"wire: $str")
  def writeToDisk(str:String) = println(s"disk: $str")
  def getFromDisk(): String = {
    println(s"read disk")
    """[["first",{"liters":7,"color":{"r":19,"g":19,"b":19},"fish":[{"color":"Red Fish","name":"One Fish"},{"color":"Blue Fish","name":"Two Fish"}]}]]"""
  }
  
  settings("first") = 7
  settings("first") = 0x13.toByte
  settings("first") = List(OneFish, TwoFish)
  
  settings.persist()
  
  val actorSystem: ActorSystem = ???
  implicit val timeout: akka.util.Timeout = ???
  implicit val ec: ExecutionContext = ???
  object asyncSettings{
    private sealed trait Message
    private case class Get(key: String)
        extends Message
    private case class SetGrey(key: String, hue: Byte)
        extends Message
        
    private class Perform extends Actor{
      override val receive: Receive = step(Map())
      def step(map: Map[String, FishTank]): Receive = {
          case Get(key) => sender ! map(key)
          case SetGrey(key, value) =>
            val newTank: FishTank = ???
            val newMap = map + (key -> newTank)
            context.become(step(newMap))
      }
      override def preStart(): Unit = ???//recall
      override def postStop(): Unit = ???//persist
    }
    
    val actor: ActorRef = actorSystem.actorOf{
      Props(new Perform())
    }
    def apply(key: String): Future[FishTank] =
      (actor ? Get(key)).collect{
        case Some(t: FishTank) => t
      }
    def update(key: String, hue: Byte) = 
      actor ! SetGrey(key, hue)
  }
}
