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
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class FunctionalStructural extends Process {

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


  /* Functional Structural Plant
   * Assume that the model begins at plant emergence, not at time of sowing.
   */

  val LateVegetativeStageSwitchThreshold = Double("c:LateVegetativeStageSwitchThreshold") default(355)
  LateVegetativeStageSwitchThreshold annotate("units", "Cd")

  val EarlyVegetativeStageGrowth = Double("c:EarlyVegetativeStageGrowth") default(30.3)
  EarlyVegetativeStageGrowth annotate("units", "Cd")

  val LateVegetativeStageGrowth = Double("c:LateVegetativeStageGrowth") default(11.9)
  LateVegetativeStageGrowth annotate("units", "Cd")

  val Total_Cd = Double("c:Total_Cd") default(0)
  Total_Cd("description", "Accumulated degree days")
  Total_Cd("units", "Cd")

  /* Every time step, add Temperature * (tau/24) to the degree day. Different from Thermaltime due to tau vs. hourly? */
  override def step(t: Double, tau: Double) {
    calc(Total_Cd) := Total_Cd + (Temperature * (tau/24))
  }

  val j = Double("c:j") default(1)
  j annotate("description", "Number of completed growth cycles")

  if (Total_Cd < LateVegetativeStageSwitchThreshold) {
    calc(j) := (Total_Cd / EarlyVegetativeStageGrowth).floor
  } else {
    calc(j) := ((Total_Cd - LateVegetativeStageSwitchThreshold) / LateVegetativeStageGrowth).floor + (LatevegetativeStageSwitchThreshold / EarlyVegetativeStageGrowth).floor
  }

  val NumberOfLeaves = Double("c:NumberOfLeaves") default(2)
  calc(NumberOfLeaves) := j + 1

}
