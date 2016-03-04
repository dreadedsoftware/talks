package com.dreadedsoftware.tls2016

import cats.Monoid
import cats.Monad
import cats.implicits._

object Cats {
  case class Team[Type](members: List[Type])
  
  object unstructured{
    implicit def adder[Arg]: Monoid[Team[Arg]] =
      new Monoid[Team[Arg]]{
        override def empty: Team[Arg] = Team(Nil)
        override def combine(
            left: Team[Arg], right: Team[Arg]): Team[Arg] = {
          val newMembers = left.members ++ right.members
          Team(newMembers)
        }
      }
    implicit def chainer: Monad[Team] = new Monad[Team]{
      override def flatMap[Arg, Ret](
          team: Team[Arg])(f: Arg => Team[Ret]): Team[Ret] = {
        val newMembers = team.members.flatMap(f(_).members)
        Team(newMembers)
      }
    }
  }
  object structured{
    implicit def adder[Arg]: Monoid[Team[Arg]] =
      new Monoid[Team[Arg]]{
        override def empty: Team[Arg] = Team(Nil)
        override def combine(
            left: Team[Arg], right: Team[Arg]): Team[Arg] = {
          val (lead1, indi1) = left.members.splitAt(2)
          val (lead2, indi2) = right.members.splitAt(2)
          val newMembers = lead1 ++ lead2 ++ indi1 ++ indi2
          Team(newMembers)
        }
      }
    implicit def chainer: Monad[Team] = new Monad[Team]{
      override def flatMap[Arg, Ret](
          team: Team[Arg])(f: Arg => Team[Ret]): Team[Ret] = {
        val (leaders, individuals) = team.members.map{member =>
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