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


class InputTSV extends CsvTimeSeries("input.tsv", time="ex:h") with VarCalc{

  /* Input data required: hourly light intensity, carbon dioxide level, and temperature.
   * Simulated 12h light, 12h dark cycles.
   */

  val t = Double("sim:t")
  t annotate("description", "Simulation time")
  t annotate("units", "hour")

  val h = Double("ex:h")
  h annotate("description", "Simulation 24-hour")
  h annotate("units", "hour")
  calc(h) := floor(t) % 24

  val Light = Double("c:Light")
  Light annotate("description", "Hourly light intensity")
  Light annotate("units", "umol/(m^2 s)")

  val CO2 = Double("c:CO2")
  CO2 annotate("description", "Hourly carbon dioxide level")
  CO2 annotate("units", "ppm")

  val Temperature = Double("c:Temperature")
  Temperature annotate("description", "Hourly temperature")
  Temperature annotate("units", "C")

  val DaylightHours = Double("c:DaylightHours") default(12)
  DaylightHours annotate("description", "Daylight hours")
  DaylightHours annotate("units", "h")

  val T_b = Double("c:T_b") default(3) param()
  T_b annotate("description", "Base temperature value")

  val Thermaltime = Double("c:Thermaltime")
  Temperature annotate("description", "Thermal time unit per hour")
  Temperature annotate("units", "Cd")
  calc(Thermaltime) := Temperature - T_b) / 24

}
