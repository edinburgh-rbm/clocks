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


class Input extends Process {

  /* Input data required: hourly light intensity, carbon dioxide level, and temperature.
   * Simulated 12h light, 12h dark cycles.
   * TODO: Need a better way of feeding a time series of inputs into a model!
   */

  val Light = Double("c:Light")
  Light annotate("description", "Hourly light intensity")
  Light annotate("units", "umol/(m^2 s)")

  val CO2 = Double("c:CO2")
  CO2 annotate("description", "Hourly carbon dioxide level")
  CO2 annotate("units", "ppm")

  val Temperature = Double("c:Temperature")
  Temperature annotate("description", "Hourly temperature")
  Temperature annotate("units", "C")

  val DaylightHours = Double("c:DaylightHours")
  DaylightHours annotate("description", "Daylight hours")
  DaylightHours annotate("units", "h")

  val T_b = Double("c:T_b") default(3) param()
  T_b annotate("description", "Base temperature value")

  val Thermaltime = Double("c:Thermaltime")
  Temperature annotate("description", "Thermal time unit per hour")
  Temperature annotate("units", "Cd")


  override def step(t: Double, tau: Double) {
    val h = ((t + tau)).floor

    if (h % 24 == 0) {
      Light := 50
      CO2 := 390
      Temperature := 21.5
    } else if (h % 24 == 1) {
      Light := 90
      CO2 := 380
      Temperature := 22.0
    } else if (h % 24 == 2) {
      Light := 98
      CO2 := 375
      Temperature := 21.8
    } else if (h % 24 == 3) {
      Light := 103
      CO2 := 372
      Temperature := 21.7
    } else if (h % 24 == 4) {
      Light := 100
      CO2 := 370
      Temperature := 21.6
    } else if (h % 24 == 5) {
      Light := 98
      CO2 := 372
      Temperature := 21.5
    } else if (h % 24 == 6) {
      Light := 102
      CO2 := 370
      Temperature := 21.2
    } else if (h % 24 == 7) {
      Light := 97
      CO2 := 373
      Temperature := 21.8
    } else if (h % 24 == 8) {
      Light := 103
      CO2 := 372
      Temperature := 21.6
    } else if (h % 24 == 9) {
      Light := 96
      CO2 := 372
      Temperature := 21.5
    } else if (h % 24 == 10) {
      Light := 100
      CO2 := 380
      Temperature := 21.0
    } else if (h % 24 == 11) {
      Light := 50
      CO2 := 390
      Temperature := 21.4
    } else if (h % 24 == 12) {
      Light := 0
      CO2 := 392
      Temperature := 20.5
    } else if (h % 24 == 13) {
      Light := 0
      CO2 := 391
      Temperature := 20.9
    } else if (h % 24 == 14) {
      Light := 0
      CO2 := 390
      Temperature := 21.0
    } else if (h % 24 == 15) {
      Light := 0
      CO2 := 390
      Temperature := 21.1
    } else if (h % 24 == 16) {
      Light := 0
      CO2 := 390
      Temperature := 21.0
    } else if (h % 24 == 17) {
      Light := 0
      CO2 := 390
      Temperature := 20.8
    } else if (h % 24 == 18) {
      Light := 0
      CO2 := 390
      Temperature := 21.1
    } else if (h % 24 == 19) {
      Light := 0
      CO2 := 392
      Temperature := 21.3
    } else if (h % 24 == 20) {
      Light := 0
      CO2 := 390
      Temperature := 21.2
    } else if (h % 24 == 21) {
      Light := 0
      CO2 := 392
      Temperature := 21.1
    } else if (h % 24 == 22) {
      Light := 0
      CO2 := 391
      Temperature := 21.1
    } else {
      Light := 0
      CO2 := 393
      Temperature := 21.0
    }

    calc(Thermaltime) := (Temperature - T_b) / 24
    DaylightHours := 12

  }

}
