package com.dreadedsoftware.tls2016

import scala.collection.mutable

object ImmutabilityAsDefault extends App{
  val buffer = mutable.Buffer[String]()
  worse(buffer)
  println(buffer)
  println(bad())
  println(good())
  println(better())
  
  def worse(fish: mutable.Buffer[String]): Unit = {
    var one = "One Fish"
    var two = "Two Fish"
    
    fish.append(one)
    fish.append(two)
    
    one = color(one)
    two = color(two)
    
    fish.append(one)
    fish.append(two)
  }
  
  def bad(): mutable.Buffer[String] = {
    val fish = mutable.Buffer[String]()
    var one = "One Fish"
    var two = "Two Fish"
    
    fish.append(one)
    fish.append(two)
    
    one = color(one)
    two = color(two)
    
    fish.append(one)
    fish.append(two)
    
    fish
  }
  
  def good(): List[String] = {
    val fish = mutable.Buffer[String]()
    val one = "One Fish"
    val two = "Two Fish"
    
    fish.append(one)
    fish.append(two)
    
    fish.append(color(one))
    fish.append(color(two))
    
    fish.toList
  }
  
  def better(): List[String] = {
    val one = "One Fish"
    val two = "Two Fish"
    
    List(
        one,
        two,
        color(one),
        color(two)
    )
  }
  
  def color(str: String): String = {
    str match{
      case "One Fish" => "Red Fish"
      case "Two Fish" => "Blue Fish"
    }
  }
}