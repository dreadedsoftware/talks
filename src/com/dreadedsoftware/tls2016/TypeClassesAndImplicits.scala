package com.dreadedsoftware.tls2016

object TypeClassesAndImplicits extends App{
  import scala.language.higherKinds
  
  object coupled{
    trait Adder[Type]{
      def add(other: Type): Type
    }
    trait Chainer[Arg, Type[Arg]]{
      def chain[Res](f: Arg => Type[Res]): Type[Res]
    }
    
    case class Team[Type](members: List[Type])
      extends Adder[Team[Type]]
      with Chainer[Type, Team]{
      override def add(other: Team[Type]): Team[Type] = {
        Team(members ++ other.members)
      }
      override def chain[Res](
          f: Type => Team[Res]): Team[Res] = {
        val list = members.flatMap(member => f(member).members)
        Team(list)
      }
    }
    
    case class TeamStructured[Type](members: List[Type])
      extends Adder[TeamStructured[Type]]
      with Chainer[Type, TeamStructured]{
      override def add(
          other: TeamStructured[Type]): TeamStructured[Type] = {
        val (lead1, indi1) = members.splitAt(2)
        val (lead2, indi2) = other.members.splitAt(2)
        TeamStructured(lead1 ++ lead2 ++ indi1 ++ indi2)
      }
      override def chain[Res](
          f: Type => TeamStructured[Res]): TeamStructured[Res] = {
        val (leaders, individuals) = members.map{member =>
          val mems = f(member).members
          mems.splitAt(2)
        }.unzip
        TeamStructured(
            leaders.flatMap {x=>x} ++
            individuals.flatMap{x=>x})
      }
    }
  }
  object uncoupled{
    trait Adder[Type[_]]{
      def add[Item](
          left: Type[Item], right: Type[Item]): Type[Item]
    }
    trait Chainer[Type[_]]{
      def chain[Item, Res](
          arg: Type[Item], f: Item => Type[Res]): Type[Res]
    }
    
    case class Team[Type](members: List[Type])
    
    object unstructured{
      implicit def adder: Adder[Team] = new Adder[Team]{
        override def add[Item](
            left: Team[Item], right: Team[Item]): Team[Item] = {
          Team(left.members ++ right.members)
        }
      }
      
      implicit def chainer: Chainer[Team] = new Chainer[Team]{
        override def chain[Item, Res](
            arg: Team[Item], f: Item => Team[Res]): Team[Res] = {
          val list = arg.members.flatMap(
              member => f(member).members)
          Team(list)
        }
      }
    }
    
    object structured{
      implicit def adder: Adder[Team] = new Adder[Team]{
        override def add[Item](
            left: Team[Item], right: Team[Item]): Team[Item] = {
          val (lead1, indi1) = left.members.splitAt(2)
          val (lead2, indi2) = right.members.splitAt(2)
          Team(lead1 ++ lead2 ++ indi1 ++ indi2)
        }
      }
      
      implicit def chainer: Chainer[Team] = new Chainer[Team]{
        override def chain[Item, Res](
            arg: Team[Item], f: Item => Team[Res]): Team[Res] = {
          val (leaders, individuals) = arg.members.map{member =>
            val mems = f(member).members
            mems.splitAt(2)
          }.unzip
          Team(
              leaders.flatMap {x=>x} ++
              individuals.flatMap{x=>x})
        }
      }
    }
  }
}