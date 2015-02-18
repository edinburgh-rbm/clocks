 /*
  *  MOIS Examples: Arabidopsis Whole-Organism (Forger-Peskin 2014)
  *  Copyright (C) 2014 University of Edinburgh School of Informatics
  *
  *  This program is free software: you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation, either version 3 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program. If not, see <http://www.gnu.org/licenses/>.
  */

package uk.ac.ed.inf.mois.millar

import uk.ac.ed.inf.mois.{Model, Process, ProcessGroup, VarCalc, Math}
import uk.ac.ed.inf.mois.ode
import uk.ac.ed.inf.mois.sched.NaiveScheduler
import scala.math
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class FunctionalStructural extends Process with VarCalc{

  val t = Double("sim:t")
  t annotate("description", "Simulation time")
  t annotate("units", "hour")

  val h = Double("sim:h")
  h annotate("description", "Simulation 24-hour")
  h annotate("units", "hour")


  /* Model inputs */

  val Temperature = Double("c:Temperature")
  Temperature annotate("description", "Hourly temperature")
  Temperature annotate("units", "C")

  val T_b = Double("c:T_b") default(3) param()
  T_b annotate("description", "Base temperature value")

  val Thermaltime = Double("c:Thermaltime")
  Temperature annotate("description", "Thermal time unit per hour")
  Temperature annotate("units", "Cd")

  /* Inputs from CDM */

  val LeafCarbon = Double("c:LeafCarbon")
  val RootCarbon = Double("c:RootCarbon")

  val LG = Double("c:L")
  LG annotate("description", "Leaf growth")


  /* Leaf object */
  case class Leaf(rlnc: Int) {

    /* Rosette leaf number */
    var rln: Int = rlnc
    /* Physiological age (thermal time elapsed since appearance) */
    var n: Double = 0
    /* Dry weight of leaf */
    var weight: Double = 0
    /* Zenithal angle of leaf */
    var a: Double = 70
    /* Leaf demand */
    var d: Double = 0
    /* Leaf area */
    var area: Double = 0

    def add_age(dn: Double) {
      n = n + dn
    }

    def add_weight(dweight: Double) {
      weight = weight + dweight
    }

  }

  /* A population of these Leaf objects */
  val Leaves = scala.collection.mutable.ArrayBuffer.empty[Leaf]


  /* Functional Structural Plant
   * Assume that the module begins at plant emergence, not at time of sowing.
   */

  val LateVegetativeStageSwitchThreshold = Double("c:LateVegetativeStageSwitchThreshold") default(355)
  LateVegetativeStageSwitchThreshold annotate("units", "Cd")

  val EarlyVegetativeStageGrowth = Double("c:EarlyVegetativeStageGrowth") default(30.3)
  EarlyVegetativeStageGrowth annotate("units", "Cd")

  val LateVegetativeStageGrowth = Double("c:LateVegetativeStageGrowth") default(11.9)
  LateVegetativeStageGrowth annotate("units", "Cd")

  val Total_Cd = Double("c:Total_Cd") default(0)
  Total_Cd annotate("description", "Accumulated degree days")
  Total_Cd annotate("units", "Cd")
  calc(Total_Cd) := Total_Cd + Thermaltime


  val j = Int("c:j") default(0)
  j annotate("description", "Number of completed growth cycles")

  val j_prov = Int("c:j_prov") param()

  val NumberOfLeaves = Int("c:NumberOfLeaves") default(0)

  val T_r = Double("c:T_r") default(963.495) param()
  T_r annotate("description", "Duration of root expansion")
  T_r annotate("units", "Cd")

  val P_r = Double("c:P_r") default(2.64) param()
  P_r annotate("description", "Root sink strength")

  val a_r = Double("c:a_r") default(13.03) param()
  val b_r = Double("c:b_r") default(9.58) param()

  val M_r = Double("c:M_r") default(0.195691) param()

  /* Needs to be normalised to its maximal value M_r */
  val f_r = Double("c:f_r") param()
  f_r annotate("description", "Root sink variation")
  calc(f_r) := (1 / M_r) * (((Total_Cd + 0.5) / T_r) ** (1 / a_r)) * ((1 - ((Total_Cd + 0.5) / T_r)) ** (1 / b_r))

  val d_r = Double("c:d_r")
  d_r annotate("description", "Root demand")
  calc(d_r) := P_r * f_r


  /* Technically this should be 300 for cotelydons (first two leaves) and 400 for rest, but hey. */
  val T_l = Double("c:T_l") default(400) param()
  T_l annotate("description", "Duration of leaf expansion")
  T_l annotate("units", "Cd")

  val P_l = Double("c:P_l") default(1) param()
  P_l annotate("description", "Leaf sink strength")

  val a_l = Double("c:a_l") default(3.07) param()
  val b_l = Double("c:b_l") default(5.59) param()
  val M_l = Double("c:M_l") default(0.406873) param()

  /* Needs to be normalised to its maximal value M */
  val f_l = Double("c:f_r") param()
  f_l annotate("description", "Leaf sink variation")

  val d_l = Double("c:d_l") param()
  d_l annotate("description", "Leaf demand")

  val sumd_l = Double("c:sumd_l")
  sumd_l annotate("description", "Total leaf demand")



  val RS = Double("c:RS") default(0.12)
  RS annotate("description", "Root-to-shoot allocation ratio")
  calc(RS) := (d_r / sumd_l) * (RootCarbon / LeafCarbon)


  val delta_q = Double("c:delta_q") param()
  delta_q annotate("description", "Change in leaf biomass")

  val SLA = Double("c:SLA") param()
  SLA annotate("description", "Specific Leaf Area")
  SLA annotate("units", "m^2 / g")

  val RosetteArea = Double("c:RosetteArea")
  RosetteArea annotate("units", "m^2")

  val weight_max = Double("c:weight_max") default(0) param()
  weight_max annotate("description", "Weight of largest leaf")

  val i_max = Double("c:imax") default(1) param()
  i_max annotate("description", "Largest leaf")

  /* Calculate 13 largest functional leaves for RosetteArea. */
  val x = Double("temp:x") default(0)

  override def step(t: Double, tau: Double) {
    /* Initialise cotelydons if j=0, else spawn new leaf if appropriate stage in growth */
    if (Total_Cd < LateVegetativeStageSwitchThreshold) {
      j_prov := Math.floor(Total_Cd / EarlyVegetativeStageGrowth).toInt
      if (j == 0) {
        Leaves.append(new Leaf(1))
        Leaves.append(new Leaf(2))
        j := 1
        NumberOfLeaves := 2
      } else if (j_prov == j + 1) {
        Leaves.append(new Leaf(NumberOfLeaves + 1))
        j := j_prov
        NumberOfLeaves := j + 1
      }
    } else {
      j_prov := Math.floor(LateVegetativeStageSwitchThreshold / EarlyVegetativeStageGrowth).toInt + Math.floor((Total_Cd - LateVegetativeStageSwitchThreshold) / LateVegetativeStageGrowth).toInt
      if (j_prov == j + 1) {
        Leaves.append(new Leaf(NumberOfLeaves + 1))
        j := j_prov
        NumberOfLeaves := j + 1
      }
    }

    /* For each Leaf in Leaves calculate its demand and add to sumd_l */
    for (Leaf <- Leaves) {
      f_l := (1 / M_l) * (((Leaf.n + 0.5) / T_l) ** (1 / a_l)) * ((1 - ((Leaf.n + 0.5) / T_l)) ** (1 / b_l))
      d_l := P_l * f_l
      Leaf.d = d_l
      sumd_l := sumd_l + d_l
    }

    /* Update leaf size based on trophic competition */
    for (Leaf <- Leaves) {
      /* Does 'total increment in shoot dry mass' = LeafGrowth? */
      delta_q := (Leaf.d / sumd_l) * LG
      Leaf.add_age(Thermaltime)
      Leaf.add_weight(delta_q)

      SLA := 0.144 * Math.exp(-0.002 * Total_Cd)
      /* Leaf area does not shrink in later time points. */
      if (Leaf.area < SLA) {
        Leaf.area = SLA
      }

      /* Find largest leaf */
      if (Leaf.weight > weight_max) {
        weight_max := Leaf.weight
        i_max := Leaf.rln
      }
    }

    /* Update leaf zenithal angle */
    for (Leaf <- Leaves) {
      if (Leaf.rln <= i_max) {
        Leaf.a = 10
      } else {
        Leaf.a = 10 + (60 * (Leaf.rln - i_max) / (NumberOfLeaves - i_max))
      }
    }


    RosetteArea := 0
    for (Leaf <- Leaves) {
      x := 0
      for (k <- 0 to 12) {
        x := x + (Leaf.area * Math.cos(Leaf.a))
      }
      if (x > RosetteArea) {
        RosetteArea := x
      }
    }

    /*  (0 until (NumberOfLeaves-12)).map { k =>
     *    (0 until 12).map { leaf =>
     *      (leaf.area * Math.cos(leaf.a))
     *    }.sum
     *  }.max
     */
    super.step(t, tau)
  }
}
