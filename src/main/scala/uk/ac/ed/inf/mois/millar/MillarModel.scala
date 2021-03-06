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
import uk.ac.ed.inf.mois.sched.NaiveScheduler
import uk.ac.ed.inf.mois.sched.CompositionScheduler
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class MillarModel extends Model {

  val process = new ProcessGroup {
    scheduler = new CompositionScheduler(1.00)
  }
  process += new InputTSV()
  process += new Photoperiodism()
  process += new Photothermal()
  process += new CarbonDynamic()

  var fspm = new FunctionalStructural()
  fspm.addStepHandler(new OutputPlant())
  process += fspm

}
