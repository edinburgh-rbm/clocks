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

import uk.ac.ed.inf.mois.{Model, Process, ProcessGroup, VarCalc, Math}
import uk.ac.ed.inf.mois.ode
import uk.ac.ed.inf.mois.sched.NaiveScheduler
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class MCCModel extends Model {

  val process = new ProcessGroup {
    scheduler = new NaiveScheduler(0.01)
  }
  process += new MCCMonolithic()
  process += new MCCLight()

}
