package com.nomadiccoders.utility

import scala.language.postfixOps
/**
  * Created by jay on 21/09/2016.
  */
object Units {

  val LessThan = -1
  val Equal = 0
  val GreaterThan = 1

  def max[T <: Ordered[T]](a: T, b: T): T = if(a <= b) b  else a

  implicit class Digital$Units(underLying: Int){

    def byte: Int = underLying
    def bytes: Int = byte

    def kiloByte: Int = underLying * 1024
    def kiloBytes: Int = kiloByte
    def kb: Int = kiloBytes

    def megaByte: Int = kiloByte * 1024
    def megaBytes: Int = megaByte
    def mb = megaByte

  }

  sealed trait $Unit extends {
    self =>

    override def toString = self.getClass.getSimpleName.replace("$", "")
  }

  trait OrderedUnit[U <: $Unit] extends Ordered[U]

  trait Valuable[V, U <: $Unit]{

    def value: V

    def unit: U
    
    def to($Unit: U): Valuable[V, U]

    def +(other: Valuable[V, U]) : Valuable[V, U]
    def -(other: Valuable[V, U]) : Valuable[V, U]
    def *(other: Valuable[V, U]) : Valuable[V, U]
    def /(other: Valuable[V, U]) : Valuable[V, U]
  }

  sealed trait MassUnit extends $Unit with Ordered[MassUnit]{
    def convertTo(mass: Mass, unit: MassUnit): Mass

  }

  object Grams extends MassUnit {

    override def convertTo(mass: Mass, $Unit: MassUnit): Mass = (mass, $Unit) match {
      case (Mass(v, Grams), Grams) => mass
      case (Mass(v, Grams), KiloGrams) => Mass(v * 0.001, KiloGrams)
      case (Mass(v, Grams), Pounds) => Mass(v * 0.00220462, Pounds)
      case (Mass(v, Grams), Stone) => Mass(v * 0.000157472857142857152, Stone)
      case v => throw new IllegalArgumentException(v.toString)
    }

    override def compare(that: MassUnit): Int = that match{
      case Grams => Equal
      case _ => LessThan
    }
  }
  object KiloGrams extends MassUnit {
    override def convertTo(mass: Mass, $Unit: MassUnit): Mass = (mass, $Unit) match {
      case (Mass(v, KiloGrams), KiloGrams) => mass
      case (Mass(v, KiloGrams), Grams) => Mass(v * 1000, Grams)
      case (Mass(v, KiloGrams), Pounds) => Mass(v * 2.20462, Pounds)
      case (Mass(v, KiloGrams), Stone) => Mass(v * 0.157472857135078, Stone)
      case v => throw new IllegalArgumentException(v.toString)
    }

    override def compare(that: MassUnit): Int = that match{
      case Grams | Pounds => GreaterThan
      case KiloGrams => Equal
      case Stone => LessThan
    }
  }
  object Pounds extends MassUnit {
    override def convertTo(mass: Mass, $Unit: MassUnit): Mass = (mass, $Unit) match {
      case (Mass(v, Pounds), Pounds) => mass
      case (Mass(v, Pounds), Grams) => Mass(v * 453.59183054259443679, Grams)
      case (Mass(v, Pounds), KiloGrams) => Mass(v * 0.453592, KiloGrams)
      case (Mass(v, Pounds), Stone) => Mass(v * 0.0714286, Stone)
      case v => throw new IllegalArgumentException(v.toString)
    }

    override def compare(that: MassUnit): Int = that match {
      case Grams => GreaterThan
      case Pounds => Equal
      case KiloGrams | Stone => LessThan
    }
  }
  object Stone extends MassUnit{
    override def convertTo(mass: Mass, $Unit: MassUnit): Mass = (mass, $Unit) match {
      case (Mass(v, Stone), Stone) => mass
      case (Mass(v, Stone), Grams) => Mass(v * 6350.28999977318, Grams)
      case (Mass(v, Stone), KiloGrams) => Mass(v * 6.35029, KiloGrams)
      case (Mass(v, Stone), Pounds) => Mass(v * 14, Stone)
      case v => throw new IllegalArgumentException(v.toString)
    }

    override def compare(that: MassUnit): Int = that match {
      case Stone => Equal
      case _ => GreaterThan // TODO not greater than a Ton
    }
  }

  case class Mass(value: Double, unit: MassUnit) extends Valuable[Double, MassUnit] {
    override def to(unit: MassUnit): Mass = this.unit.convertTo(this, unit)

    override def +(other: Valuable[Double, MassUnit]): Mass = (this, other) match {
      case (m1 @ Mass(_, u1), m2 @ Mass(_, u2)) =>
        val u = max(u1, u2)
        val v = m1.to(u).value + m2.to(u).value
        Mass(v, u)
      case _ => throw new IllegalArgumentException(s"$other is not of type mass")
    }

    override def /(other: Valuable[Double, MassUnit]): Mass = (this, other) match {
      case (m1 @ Mass(_, u1), m2 @ Mass(_, u2)) =>
        val u = max(u1, u2)
        val v = m1.to(u).value / m2.to(u).value
        Mass(v, u)
      case _ => throw new IllegalArgumentException(s"$other is not of type mass")
    }

    override def -(other: Valuable[Double, MassUnit]): Mass = (this, other) match {
      case (m1 @ Mass(_, u1), m2 @ Mass(_, u2)) =>
        val u = max(u1, u2)
        val v = m1.to(u).value - m2.to(u).value
        Mass(v, u)
      case _ => throw new IllegalArgumentException(s"$other is not of type mass")
    }

    override def *(other: Valuable[Double, MassUnit]): Mass = (this, other) match {
      case (m1 @ Mass(_, u1), m2 @ Mass(_, u2)) =>
        val u = max(u1, u2)
        val v = m1.to(u).value * m2.to(u).value
        Mass(v, u)
      case _ => throw new IllegalArgumentException(s"$other is not of type mass")
    }
  }

  implicit class MassUnits(underLying: Double){

    def gram: Mass = Mass(underLying, Grams)
    def grams: Mass = gram
    def g: Mass = gram

    def kiloGram: Mass = Mass(underLying, KiloGrams)
    def kiloGrams: Mass = kiloGram
    def kg: Mass = kiloGram

    def pound: Mass = Mass(underLying, Pounds)
    def pounds: Mass = pounds
    def lbs: Mass = pounds

    def stone: Mass = Mass(underLying, Stone)

  }

  def main(args: Array[String]): Unit = {
    val res = ((95000.0 grams) + (5 kiloGrams)) / (2 kg) to Stone
    println(res)
    println(max(Pounds, KiloGrams))
  }
}
