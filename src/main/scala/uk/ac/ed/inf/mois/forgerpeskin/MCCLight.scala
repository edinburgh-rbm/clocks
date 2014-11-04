 /*
  *  MOIS Examples: Mammalian Circadian Clock (Forger-Peskin 2002)
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

package uk.ac.ed.inf.mois.forgerpeskin

import uk.ac.ed.inf.mois.{Model, ODE, Process, ProcessGroup, VarCalc, Math}
import uk.ac.ed.inf.mois.sched.NaiveScheduler
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class MCCLight extends Process {

  val L = Double("c:L")
  L annotate("description", "Effect of light level on transcription")

 /*
  * val Lon = Double("c:Lon") default(0.000339)
  * Lon annotate("description", "Light level (light on)")
  *
  * val Loff = Double("c:Loff") default(0)
  * Loff annotate("description", "Light level (light off)")
  *
  * If Math.floor([T]/12) is odd, use Loff as input to MammalianCircadianClock.
  * Else, use Lon.
  *
  */

  override def step(t: Double, tau: Double) {
    if (((t + tau)/12).floor % 2 == 0) {
      L := 0.000339
    } else {
      L := 0
    }
  }
}