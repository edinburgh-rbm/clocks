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


class CarbonDynamic extends Process with VarCalc{

  val t = Double("sim:t")
  t annotate("description", "Simulation time")
  t annotate("units", "hour")

  val h = Double("sim:h")
  h annotate("description", "Simulation 24-hour")
  h annotate("units", "hour")


  /* Model inputs */

  val Light = Double("c:Light")
  Light annotate("description", "Hourly light intensity")
  Light annotate("units", "umol/(m^2 s)")

  val CO2 = Double("c:CO2")
  CO2 annotate("description", "Hourly carbon dioxide level")
  CO2 annotate("units", "ppm")

  val Temperature = Double("c:Temperature")
  Temperature annotate("description", "Hourly temperature")
  Temperature annotate("units", "C")


  /* Inputs from FSPM */

  val RosetteArea = Double("c:RosetteArea")

  val RS = Double("c:RS") default(0.12)
  RS annotate("description", "Root-to-shoot allocation ratio")


  /* Carbon Assimilation */

  val R = Double("c:R") default(8.314) param()
  R annotate("description", "Gas constant")
  R annotate("units", "J/Kmol")

  val K_c25 = Double("c:K_c25") default(40.4) param()
  K_c25 annotate("description", "Michaelis constant of carboxylation at 25C")
  K_c25 annotate("units", "Pa")

  val H_c = Double("c:H_c") default(59400) param()
  H_c annotate("description", "Activation energy for carboxylation constant")
  H_c annotate("units", "J/mol")

  val K_c = Double("c:K_c")
  K_c annotate("description", "Michaelis constant of carboxylation")
  K_c annotate("units", "Pa")
  calc(K_c) := K_c25 * Math.exp((H_c * (Temperature - 25)) / (298 * R * (Temperature + 273)))

  val K_o25 = Double("c:K_o25") default(24800) param()
  K_o25 annotate("description", "Michaelis constant of oxygenation at 25C")
  K_o25 annotate("units", "Pa")

  val H_o = Double("c:H_o") default(36000) param()
  H_o annotate("description", "Activation energy for oxygenation constant")
  H_o annotate("units", "J/mol")

  val K_o = Double("c:K_o")
  K_o annotate("description", "Michaelis constant of oxygenation")
  K_o annotate("units", "Pa")
  calc(K_o) := K_o25 * Math.exp((H_o * (Temperature - 25)) / (298 * R * (Temperature + 273)))

  val V_cmax25 = Double("c:V_cmax25") default(29.6875) param()
  V_cmax25 annotate("description", "Maximum carboxylation rate at 25C")
  V_cmax25 annotate("units", "umol/ms")

  val H_V = Double("c:H_V") default(64800) param()
  H_V annotate("description", "Activation energy for maximum carboxylation rate")
  H_V annotate("units", "J/mol")

  val V_cmax = Double("c:V_cmax")
  V_cmax annotate("description", "Maximum rate of carboxylation")
  V_cmax annotate("units", "umol/ms")
  calc(V_cmax) := V_cmax25 * Math.exp((H_V * (Temperature - 25)) / (298 * R * (Temperature + 273)))


  val O2i = Double("c:O2i") default(20500) param()
  O2i annotate("description", "O2 partial pressure")
  O2i annotate("units", "Pa")

  val CO2i = Double("c:CO2i")
  CO2i annotate("description", "CO2 partial pressure")
  CO2i annotate("units", "Pa")
  calc(CO2i) := CO2 * p1

  val p1 = Double("c:p1") default(0.7) param()
  p1 annotate("description", "Ratio of intercellular to ambient CO2 level")

  val p2 = Double("c:p2") default(3.69) param()
  p2 annotate("description", "Quadratic constant of the CO2 compensation point")
  p2 annotate("units", "Pa")

  val p3 = Double("c:p3") default(0.188) param()
  p3 annotate("description", "Quadratic constant of the CO2 compensation point")
  p3 annotate("units", "Pa/K")

  val p4 = Double("c:p4") default(0.0036) param()
  p4 annotate("description", "Quadratic constant of the CO2 compensation point")
  p4 annotate("units", "Pa/K^2")

  val Gamma_star = Double("c:Gamma_star")
  Gamma_star annotate("description", "CO2 compensation point in absence of mitochondrial respiration")
  Gamma_star annotate("units", "Pa")
  calc(Gamma_star) := p2 + (p3 * (Temperature - 25)) + (p4 * (Temperature - 25) * (Temperature - 25))

  val A_c = Double("c:A_c")
  A_c annotate("description", "Rate of assimilation limited by RuBisCO")
  calc(A_c) := V_cmax * ((CO2i - Gamma_star) / (CO2i + (K_c * (1 + (O2i / K_o)))))


  val H_J = Double("c:H_J") default(37000) param()
  H_J annotate("description", "Activation energy for maximum electron transport")
  H_J annotate("units", "J/mol")

  val R_JV = Double("c:R_JV") default(1.7) param()
  R_JV annotate("description", "Ratio between Jmax and Vmax for 12h photoperiod")

  val J_max25 = Double("c:J_max25")
  J_max25 annotate("description", "Potential rate of electron transport at 25C")
  calc(J_max25) := R_JV * V_cmax25

  val p5 = Double("c:p5") default(710) param()
  p5 annotate("description", "Temperature response parameter for electron transport")
  p5 annotate("units", "J/Kmol")

  val p6 = Double("c:p6") default(220000) param()
  p6 annotate("description", "Curvature parameter of J_max")
  p6 annotate("units", "J/mol")

  val J_max = Double("c:J_max")
  J_max annotate("description", "Potential rate of electron transport")
  calc(J_max) := J_max25 * ((Math.exp((H_J * (Temperature - 25)) / (298 * R * (Temperature + 273))) * (1 + Math.exp(((298 * p5) - p6) / (298 * R)))) / (1 + Math.exp(((Temperature + 273) * p5) - p6) / ((Temperature + 273) * R)))

  /* PAR left undefined in supplementary info. Assume black-body source at 5800K (i.e. sun)?
   * With no further information for now, assume PAR is equivalent to Light input. May need to update calculations later.
   */
  val PAR = Double("c:PAR")
  PAR annotate("description", "Irradiance")
  PAR annotate("units", "(umol photon)/(m^2 s)")
  calc(PAR) := Light

  val f_spec = Double("c:f_spec") default(0.15) param()
  f_spec annotate("description", "Spectral correction factor due to absorbance of irradiance by tissues other than the chloroplast lamella")

  val p7 = Double("c:p7") default(0.7) param()
  p7 annotate("description", "Curvature of electron transport in response to irradiance")

  /* Manually solving the quadratic function: calc(J) := (p7 * J * J) - (J_b * J) - J_c = 0 
   * Use the larger of the two roots, or 0 if no real solution exists.
   */
  val J_b = Double("c:J_b") param()
  calc(J_b) := (((PAR * (1 - f_spec)) / 2) + J_max)

  val J_c = Double("c:J_c") param()
  calc(J_c) := (((PAR * (1 - f_spec)) / 2) * J_max)

  val J_discriminant = Double("c:J_discriminant") param()
  calc(J_discriminant) := Math.sqrt((J_b * J_b) - (4 * p7 * J_c))

  val J_1 = Double("c:J_1") param()
  calc(J_1) := ((0 - J_b) + J_discriminant) / (2 * p7)

  val J_2 = Double("c:J_2") param()
  calc(J_2) := ((0 - J_b) - J_discriminant) / (2 * p7)

  val J = Double("c:J")
  J annotate("description", "Rate of electron transport")
  calc(J) := {
    if (J_discriminant >= 0) {
      if (J_1 >= J_2) {
        J_1
      } else {
        J_2
      }
    } else {
      0
    }
  }

  val A_j = Double("c:A_j")
  A_j annotate("description", "Rate of assimilation limited by electron transport")
  calc(A_j) := (J * (CO2i - Gamma_star)) / (4 * (CO2i - (2 * Gamma_star)))


  val A_net = Double("c:A_net")
  calc(A_net) := {
    if (J == 0) {
      A_c
    } else {
      Math.min(A_c, A_j)
    }
  }

  val CarbonAssimilation = Double("c:CarbonAssimilation")
  calc(CarbonAssimilation) := 12 * 0.000001 * 3600 * A_net * RosetteArea


  /* Carbon Partitioning */

  val StarchCarbon = Double("c:StarchCarbon")
  val SugarCarbon = Double("c:SugarCarbon")
  val LeafCarbon = Double("c:LeafCarbon")
  val RootCarbon = Double("c:RootCarbon")


  val ST_br = Double("c:ST_br") default(0.125) param()
  ST_br annotate("description", "Baseline starch production coefficient")

  val ST_c = Double("c:ST_c") default(0.84) param()
  ST_c annotate("description", "Proportion of night-time starch breakdown")

  /* Starch-sugar partitioning differs between day and night. Assume h=0-11 is daytime and h=12-23 is night (implicit: 12/12 photoperiod). */
  val StarchSynthesis = Double("c:StarchSynthesis") default(0)
  val PartitionToSugar = Double("c:PartitionToSugar") default(0)
  val StarchDegradation = Double("c:StarchDegradation") default(0)
  val StarchCarbonEOD = Double("c:StarchCarbonEOD") default(0)
  val St = Double("c:St") default(0)

  val p8 = Double("c:p8") default(0.085) param()
  p8 annotate("description", "Leaf respiration per unit sucrose")

  val p9 = Double("c:p9") default(0.016) param()
  p9 annotate("description", "Basal leaf respiration")
  p9 annotate("units", "gC/m^2")

  /* Sugar content per unit rosette area = SugarCarbon / RosetteArea ? */
  val R_t20 = Double("c:R_t20")
  R_t20 annotate("description", "Leaf maintenance respiration per unit area at 20C")
  calc(R_t20) := (p8 * (SugarCarbon / RosetteArea)) + p9

  val H_r = Double("c:H_r") default(66400) param()
  H_r annotate("description", "Activation energy for leaf respiration")
  H_r annotate("units", "J/mol")

  val R_t = Double("c:R_t")
  R_t annotate("description", "Leaf maintenance respiration per unit area")
  calc(R_t) := R_t20 * Math.exp((H_r * (Temperature - 20)) / (293 * R * (Temperature + 273)))

  val R_above = Double("c:R_above")
  R_above annotate("description", "Total leaf maintenance respiration")
  calc(R_above) := R_t * RosetteArea

  val R_below = Double("c:R_below")
  R_below annotate("description", "Total root maintenance respiration")
  calc(R_below) := R_t * (RootCarbon / LeafCarbon)


  val Q_trans = Double("c:Q_trans")
  Q_trans annotate("description", "Transient carbon available for growth")
  calc(Q_trans) := SugarCarbon + St - R_above - R_below

  val SSU_min = Double("c:SSU_min") default(0.05) param()
  SSU_min annotate("description", "Minimum sugar content in the leaves")
  SSU_min annotate("units", "g sugar-C / m^2")

  val C_avail = Double("c:C_avail")
  C_avail annotate("description", "Carbon availability")
  calc(C_avail) := Q_trans - (SSU_min * RosetteArea)

  val Q_C = Double("c:Q_C") default(0)
  Q_C annotate("description", "Carbon available for growth")

  val TransL = Double("c:TransL") default(0)
  TransL annotate("description", "Translocation from leaf carbon")

  val TransR = Double("c:TransR") default(0)
  TransR annotate("description", "Translocation from root carbon")


  val GR_max = Double("c:GR_max") default(0.408) param()
  GR_max annotate("description", "Maximum relative growth rate")
  GR_max annotate("units", "g / g d")

  val d_L = Double("c:d_L")
  d_L annotate("description", "Leaf growth demand per time step")
  d_L annotate("units", "g C / h")
  calc(d_L) := GR_max * LeafCarbon / 24

  val alpha = Double("c:alpha") default(0.195) param()
  alpha annotate("description", "Growth respiration coefficient")

  val d_RL = Double("c:d_RL")
  d_RL annotate("description", "Required leaf growth respiration")
  calc(d_RL) := (d_L * alpha) / (1 - alpha)

  /* Replaced by RS from FSPM!!
   *
   * val RS = Double("c:RS") default(0.12)
   * RS annotate("description", "Root-to-shoot allocation ratio")
   * if (RosetteArea < 0.000122) {
   *   RS := 0.12
   * } else if (RosetteArea < 0.000396 {
   *   RS := 0.0496 + (555 * RosetteArea)
   * } else {
   *   RS := 0.27
   * }
   */

  val d_R = Double("c:d_R")
  d_R annotate("description", "Root growth demand per time step")
  calc(d_R) := RS * d_L

  val d_RR = Double("c:d_RR")
  d_RR annotate("description", "Required root growth respiration")
  calc(d_RR) := (d_R * alpha) / (1 - alpha)

  val D_C = Double("c:D_C")
  D_C annotate("description", "Total growth demand for carbon")
  calc(D_C) := d_L + d_RL + d_R + d_RR


  val LG = Double("c:L")
  LG annotate("description", "Leaf growth")

  val R_ml = Double("c:R_ml")
  R_ml annotate("description", "Leaf growth respiration")

  val RG = Double("c:R")
  RG annotate("description", "Root growth")

  val R_mr = Double("c:R_mr")
  R_mr annotate("description", "Root growth respiration")

  val O_sta = Double("c:O_sta") default(0)
  O_sta annotate("description", "Overflow transferring excess carbon to starch pool")

  calc(O_sta) := {
    if (h < 12) {
      Q_C - D_C
    } else {
      0
    }
  }

  override def step(t: Double, tau: Double) {
    if (h < 12) {
      StarchSynthesis := ST_br * CarbonAssimilation
      PartitionToSugar := CarbonAssimilation - StarchSynthesis
      St := PartitionToSugar
    } else {
      if (h == 12) {
        StarchCarbonEOD := StarchCarbon
      }
      StarchDegradation := (ST_c * StarchCarbonEOD) / 12
      St := StarchDegradation
    }
    LeafCarbon := LeafCarbon + LG - TransL
    RootCarbon := RootCarbon + RG - TransR
    StarchCarbon := StarchCarbon + StarchSynthesis - StarchDegradation + O_sta
    SugarCarbon := SugarCarbon + St - R_above - R_below - R_ml - R_mr - O_sta - LG - RG + TransL + TransR

    if (C_avail >= 0) {
      Q_C := C_avail
    } else {
      TransL := - C_avail * (LeafCarbon / (LeafCarbon + RootCarbon))
      TransR := - C_avail * (RootCarbon / (LeafCarbon + RootCarbon))
    }

    if (D_C <= Q_C) {
      LG := d_L
      R_ml := d_RL
      RG := d_R
      R_mr := d_RR
    } else {
      LG := (d_L / D_C) * Q_C
      R_ml := (d_RL / D_C) * Q_C
      RG := (d_R / D_C) * Q_C
      R_mr := (d_RR / D_C) * Q_C
    }
    super.step(t, tau)
  }
}
