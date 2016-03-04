package com.dreadedsoftware.tls2016

import ImmutabilityAsDefault.color

object CaseClasses extends App {
  val oneFish = CaseFish("One Fish")
  val twoFish = oneFish.copy(fishName = "Two Fish")
  
  println(oneFish + " " + oneFish.fishColor)
  println(twoFish + " " + twoFish.fishColor)
  
  case class CaseFish(fishName: String){
    val fishColor = color(fishName)
  }
  
  object SimpleSub{
    sealed trait Fish{
      val name: String
      val color: String
    }
    case object OneFish extends Fish{
      override final val name: String = "One Fish"
      override final val color: String = "Red Fish"
    }
    case object TwoFish extends Fish{
      override final val name: String = "Two Fish"
      override final val color: String = "Blue Fish"
    }
    
    case object NotFish extends Fish{
      override final val name: String = "Ahab"
      override final val color: String = "White Whale"
    }
  }
  
  object CombinatorialSub{
    sealed trait Fish{
      val name: String
      val color: String
    }
    sealed trait One extends Fish{
      override final val name: String = "One Fish"
    }
    sealed trait Two extends Fish{
      override final val name: String = "Two Fish"
    }
    sealed trait Red extends Fish{
      override final val color: String = "Red Fish"
    }
    sealed trait Blue extends Fish{
      override final val color: String = "Blue Fish"
    }
    object OneFish extends Fish with One with Red
    object TwoFish extends Fish with Two with Blue
    
    object NotFish extends Fish{
      override final val name: String = "Ahab"
      override final val color: String = "White Whale"
    }
  }
  
  object SelfSub{
    sealed trait Fish{
      val name: String
      val color: String
    }
    sealed trait One{self: Fish =>
      override final val name: String = "One Fish"
    }
    sealed trait Two{self: Fish =>
      override final val name: String = "Two Fish"
    }
    sealed trait Red{self: Fish =>
      override final val color: String = "Red Fish"
    }
    sealed trait Blue{self: Fish =>
      override final val color: String = "Blue Fish"
    }
    object OneFish extends Fish with One with Red
    object TwoFish extends Fish with Two with Blue
    
    object NotFish extends Fish{
      override final val name: String = "Ahab"
      override final val color: String = "White Whale"
    }
  }
}
