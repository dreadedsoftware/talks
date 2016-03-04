package com.dreadedsoftware.tls2016

import ImmutabilityAsDefault.color

object CombinatorsRule extends App{
  
  val oneFish = new Fish("One Fish")
  val twoFish = new Fish("Two Fish")
  
  val blueFish = oneFish.spawnFish(change)
  val redFish = twoFish.spawnFish(change)
  
  def change(name: String) = name match{
    case "One Fish" => "Two Fish"
    case "Two Fish" => "One Fish"
  }
  
  class BadFish(
      private var m_name: String,
      private var m_color: String
  ){
    def this() = this(null, null)
    
    def getName(): String = m_name
    def getColor(): String = m_name
    def setName(name: String){
      m_name = name
    }
    def setColor(color: String){
      m_color = color
    }
    
    def isValid(): Boolean = try{
      check()
      true
    }catch{
      case _: IllegalArgumentException => false
    }
    
    def check(): Unit = {
      check(m_name, m_color)
    }
    def check(newName: String, newColor: String){
      if(!color(newName).equals(newColor))
        throw new IllegalArgumentException(
            "Fish color and name do not match"
        )
    }
  }
  
  class Fish(val fishName: String){
    val fishColor: String = color(fishName)
    def spawnFish(f: String => String): Fish = {
      new Fish(f(fishName))
    }
  }
  
}
