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

import uk.ac.ed.inf.mois.{Model, ODE, Process, ProcessGroup, VarCalc, Math}
import uk.ac.ed.inf.mois.sched.NaiveScheduler
import scala.math
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class Photothermal extends Process with VarCalc {

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
  Thermaltime annotate("description", "Thermal time unit per hour")
  Thermaltime annotate("units", "Cd")


  /* Inputs from PPM - need to work out how FTarea is calculated there... */
  val FTarea = Double("c:FTarea")


  /* Photoperiod */

  /* Replaced by PPM
   * val CSDL = Double("c:CSDL") default(10) param()
   * CSDL annotate("description", "Critical short day length")
   * CSDL annotate("units", "h")
   * 
   * val CLDL = Double("c:CLDL") default(14) param()
   * CLDL annotate("description", "Critical long day length")
   * CLDL annotate("units", "h")
   *
   * val D_LD = Double("c:D_LD") default(1) param()
   * D_LD annotate("description", "Maximum rate for photoperiod effect to bolting")
   *
   * val D_SD = Double("c:D_SD") default(1) param()
   * D_SD annotate("description", "Minimum rate for photoperiod effect to bolting")
   *
   * val DL = Double("c:DL") default(12) param()
   * DL annotate("description", "Day length - input from model parameters, default 12")
   *
   * val Photoperiod = Double("c:Photoperiod")
   * if (DL <= CSDL) {
   *   Photoperiod := D_SD
   * } else if (DL >= CLDL) {
   *   Photoperiod := D_LD
   * } else {
   *   calc(Photoperiod) := D_SD + (((DL - CSDL) * (D_LD - D_SD)) / (CLDL - CSDL))
   * }
   */

  val A = Double("c:A") default(1) param()
  val B = Double("c:B") default(-0.3635) param()
  val C = Double("c:C") default(5.733241) param()
  val n = Double("c:n") default(7) param()

  val Photoperiod = Double("c:Photoperiod")
  calc(Photoperiod) := A + (B * ((Math.pow(C, n)) / ((Math.pow(C, n)) + (Math.pow(FTarea, n)))))


  /* Thermal */

  val P_day = Double("c:P_day") default(1) param()
  P_day annotate("description", "Gating function to account for sensitivity to day temperature")

  val P_night = Double("c:P_night") default(0.1782) param()
  P_night annotate("description", "Gating function to account for sensitivity to night temperature")

  val P_t = Double("c:P_t")
  if (h < 12) {
    calc(P_t) := P_day
  } else {
    calc(P_t) := P_night
  }

  val Thermal = Double("c:Thermal")
  if (Temperature >= T_b) {
    calc(Thermal) := P_t * (Temperature - T_b)
  } else {
    Thermal := 0
  }


  /* Vernalisation */

  val T_Vmin = Double("c:T_Vmin") default(-3.5) param()
  T_Vmin annotate("description", "Minimum vernalising temperature")
  T_Vmin annotate("units", "C")

  val T_Vmax = Double("c:T_Vmax") default(6) param()
  T_Vmax annotate("description", "Maximum vernalising temperature")
  T_Vmax annotate("units", "C")

  val Kappa = Double("c:Kappa") default(-5.17) param()
  val Omega = Double("c:Omega") default(2.23) param()
  val Epsilon = Double("c:Epsilon") default(1.00) param()

  val V_e = Double("c:V_e")
  V_e annotate("description", "Vernalisation effectiveness")
  calc(V_e) := Math.exp(Kappa) * ((Temperature - T_Vmin) ** Omega) * ((T_Vmax - Temperature) ** Epsilon)

  /* Need to verify this */
  val V_h = Double("c:V_h") default(0)
  V_h annotate("description", "Cumulative vernalisation hours")
  calc(V_h) := V_h + V_e

  val V_sat = Double("c:V_sat") default(960) param()
  V_sat annotate("description", "Saturation point at which FLC is permanently inactivated")

  val F_b = Double("c:F_b") default(0.4743) param()
  F_b annotate("description", "Baseline FLC repression")

  val Vernalisation = Double("c:Vernalisation")
  if (V_h < V_sat) {
    calc(Vernalisation) := F_b + ((V_h * (1 - F_b)) / V_sat)
  } else {
    Vernalisation := 1
  }


  val MPTU = Double("c:MPTU")
  calc(MPTU) := Photoperiod * Thermal * Vernalisation

  val CumulativeMPTU = Double("c:CumulativeMPTU") default(0)
  calc(CumulativeMPTU) := CumulativeMPTU + MPTU

  val ThresholdMPTU = Double("c:ThresholdMPTU") default(3212) param()

  /* If CumulativeMPTU > ThresholdMPTU then flowering */ 

}
